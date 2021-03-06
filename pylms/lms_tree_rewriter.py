import sys
import ast
import types
import parser
import inspect
import builtins
import astunparse
import collections

class ScopeAnalysis(ast.NodeVisitor):
    """
    Find single-assigment variables. These correspond to
    'val x = ...' in Scala and don't need to be lifted.
    """
    def __init__(self):
        self.fundef = None
        super()

    def visit_Assign(self, node):
        assert(len(node.targets) == 1) # FIXME

        if isinstance(node.targets[0], ast.Attribute):
            self.generic_visit(node)
            return
        elif isinstance(node.targets[0], ast.Tuple):
            ids = list(map(lambda x: x.id, node.targets[0].elts))
        else:
            ids = [node.targets[0].id] # TODO: brittle, should look at shadowing, etc.

        locals = self.fundef.locals

        for id in ids:
            if not locals.get(id): locals[id] = 0
            locals[id] += 1

        self.generic_visit(node)

    def visit_FunctionDef(self, node):
        node.parent = self.fundef
        self.fundef = node
        node.locals = {}
        self.generic_visit(node)
        self.fundef = node.parent


class StagingRewriter(ast.NodeTransformer):
    """
    StagingRewriter does two things:
    1) virtualize primitives such as `if`, `while`, `for` and etc
    2) virtualize var accesses for non-single-assignment vars
    """
    def __init__(self):
        self.fundef = None # keep track of the current function we're in
        self.var_names = {}
        super()


    def freshName(self,s = ""):
        if s not in self.var_names:
            self.var_names[s] = 0
        self.var_names[s] += 1
        return "{0}${1}".format(s, self.var_names[s])

    def shouldLiftVar(self, id):
        # lift a var if it's assigned more than once
        # TODO: need to check super scopes?
        return ((self.fundef.locals.get(id)) and
               (self.fundef.locals[id] > 1))

    def visit_FunctionDef(self, node):
        node.parent = self.fundef
        self.fundef = node
        self.generic_visit(node)

        # generate code to pre-initialize staged vars
        # we stage all vars that are written to more than once
        inits = (ast.Assign(targets=[ast.Name(id=id, ctx=ast.Store())],
           value=ast.Call(func=ast.Name(id='__var', ctx=ast.Load()), args=[], keywords=[])) for id in node.locals if node.locals[id] > 1)

        new_node = ast.copy_location(ast.FunctionDef(name=node.name,
                                         args=node.args,
                                         body=[ast.Try(body=list(inits) + node.body,
                                                      handlers=[ast.ExceptHandler(type=ast.Name(id='NonLocalReturnValue', ctx=ast.Load()),
                                                                                       name='r',
                                                                                       body=[ast.Return(value=ast.Attribute(value=ast.Name(id='r', ctx=ast.Load()), attr='value', ctx=ast.Load()))])],
                                                      orelse=[],
                                                      finalbody=[])],
                                         decorator_list=list(filter(lambda n: n.id!='lms', node.decorator_list)),
                                         returns=node.returns),
                          node)
        ast.fix_missing_locations(new_node)
        self.fundef = node.parent
        return new_node

    def visit_Assign(self, node):
        assert(len(node.targets) == 1) # FIXME (doesn't work -- if multiple targets, it's a Tuple (single))

        if isinstance(node.targets[0], ast.Attribute):
            self.generic_visit(node)
            return node
        elif isinstance(node.targets[0], ast.Tuple):
            self.generic_visit(node)
            return node

        id = node.targets[0].id

        # NOTE: grab id before -- recursive call will replace lhs with __read!!
        self.generic_visit(node)

        if not self.shouldLiftVar(id):
            return node

        new_node = ast.Expr(ast.Call(
            func=ast.Name(id='__assign', ctx=ast.Load()),
            args=[ast.Name(id=id, ctx=ast.Load()),
                  node.value
                 ],
            keywords=[]
        ))
        ast.copy_location(new_node, node)
        ast.fix_missing_locations(new_node)

        return [new_node]

    def visit_Name(self, node):
        self.generic_visit(node)

        if not self.shouldLiftVar(node.id):
            return node

        new_node = ast.Call(
            func=ast.Name(id='__read', ctx=ast.Load()),
            args=[ast.Name(id=node.id, ctx=ast.Load())],
            keywords=[]
        )
        ast.copy_location(new_node, node)
        ast.fix_missing_locations(new_node)

        return new_node

    def visit_If(self, node):
        self.generic_visit(node)
        tBranch_name = self.freshName("then")
        eBranch_name = self.freshName("else")
        tBranch = ast.FunctionDef(name=tBranch_name,
                                  args=ast.arguments(args=[], vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[]),
                                  body=node.body,
                                  decorator_list=[])

        if len(node.orelse) is 0:
            node.orelse = [ast.Pass()]

        eBranch = ast.FunctionDef(name=eBranch_name,
                                  args=ast.arguments(args=[], vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[]),
                                  body=node.orelse,
                                  decorator_list=[])
        ast.fix_missing_locations(tBranch)
        ast.fix_missing_locations(eBranch)

        self.generic_visit(tBranch)
        self.generic_visit(eBranch)

        new_node = ast.Expr(value=ast.Call(
            func=ast.Name(id='__if', ctx=ast.Load()),
            args=[node.test,
                  ast.Name(id=tBranch_name, ctx=ast.Load()),
                  ast.Name(id=eBranch_name, ctx=ast.Load())
                 ],
            keywords=[]
        ))

        ast.fix_missing_locations(new_node)
        mod = [tBranch, eBranch, new_node]
        return mod

    def visit_While(self, node):
        self.generic_visit(node)

        tFun_name = self.freshName("cond")
        bFun_name = self.freshName("body")
        tFun = ast.FunctionDef(name=tFun_name,
                                  args=ast.arguments(args=[], vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[]),
                                  body=[ast.Return(node.test)],
                                  decorator_list=[])
        bFun = ast.FunctionDef(name=bFun_name,
                                  args=ast.arguments(args=[], vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[]),
                                  body=node.body,
                                  decorator_list=[])
        ast.fix_missing_locations(tFun)
        ast.fix_missing_locations(bFun)

        new_node = ast.Expr(ast.Call(
            func=ast.Name(id='__while', ctx=ast.Load()),
            args=[ast.Name(id=tFun_name, ctx=ast.Load()),
                  ast.Name(id=bFun_name, ctx=ast.Load()),
                 ],
            keywords=[]
        ))

        ast.fix_missing_locations(new_node)
        mod = [tFun, bFun, new_node]
        return mod

    def visit_Continue(self, node):
        self.generic_visit(node)

        new_node = ast.Expr(ast.Call(
            func=ast.Name(id='__continue', ctx=ast.Load()),
            args=[],
            keywords=[]
        ))

        ast.fix_missing_locations(new_node)
        return new_node

    def visit_Break(self, node):
        self.generic_visit(node)

        new_node = ast.Expr(ast.Call(
            func=ast.Name(id='__break', ctx=ast.Load()),
            args=[],
            keywords=[]
        ))

        ast.fix_missing_locations(new_node)
        return new_node

    def visit_Call(self, node):
        self.generic_visit(node)

        if isinstance(node.func, ast.Attribute) and isinstance(node.func.value, ast.Name):
            if node.func.value.id is 'nn':
                if node.func.attr is 'Linear':
                    new_node = ast.Call(func=ast.Name(id="nn_linear", ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

                if node.func.attr is 'Conv2d':
                    new_node = ast.Call(func=ast.Name(id="nn_conv2d", ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node
            elif node.func.value.id is 'optim':
                if node.func.attr is 'SGD':
                    new_node = ast.Call(func=ast.Name(id='optim_SGD', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node
            elif node.func.value.id is 'F':
                if node.func.attr is 'nll_loss':
                    new_node = ast.Call(func=ast.Name(id='F_nll_loss', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

                if node.func.attr is 'relu':
                    new_node = ast.Call(func=ast.Name(id='F_relu', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

                if node.func.attr is 'dropout':
                    new_node = ast.Call(func=ast.Name(id='F_dropout', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

                if node.func.attr is 'max_pool2d':
                    new_node = ast.Call(func=ast.Name(id='F_max_pool2d', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

                if node.func.attr is 'log_softmax':
                    new_node = ast.Call(func=ast.Name(id='F_log_softmax', ctx=ast.Load()),
                                                      args=node.args,
                                                      keywords=node.keywords)
                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    return new_node

        if not isinstance(node.func, ast.Name):
            return node

        if node.func.id == 'Variable':
            new_node = ast.Call(func=ast.Name(id='__variable', ctx=ast.Load()), args=node.args, keywords=[])
            ast.copy_location(new_node, node)
            ast.fix_missing_locations(new_node)
            return new_node

        if node.func.id == 'print':
            new_node = ast.Call(func=ast.Name(id='__print', ctx=ast.Load()),
                                              args=node.args,
                                              keywords=[])
            ast.copy_location(new_node, node)
            ast.fix_missing_locations(new_node)
            return new_node

        return node

    def visit_Subscript(self, node):
        self.generic_visit(node)

        if isinstance(node.value, ast.Attribute):
            if isinstance(node.value.value, ast.Call):
                if node.value.attr is 'data':
                # print("{}".format(ast.dump(node)))
                # if node.value.value.func.value.id is 'F' and node.value.value.attr is 'data':
                    new_node = ast.Call(func=ast.Attribute(value=node.value.value, attr='data_get', ctx=ast.Load()), args=[node.slice.value], keywords=[])
                    # new_node.value.func.attr = 'data_get'

                    ast.copy_location(new_node, node)
                    ast.fix_missing_locations(new_node)
                    print("{}".format(ast.dump(new_node)))
                    return new_node

            elif node.value.attr is 'data':
                new_node = ast.Call(func=ast.Attribute(value=node.value.value, attr='data_get', ctx=ast.Load()), args=[node.slice.value], keywords=[])
                ast.copy_location(new_node, node)
                ast.fix_missing_locations(new_node)
                print("{}".format(ast.dump(new_node)))
                return new_node

        return node

    def visit_Return(self, node):
        self.generic_visit(node)
        new_node = ast.Expr(ast.Call(func=ast.Name(id='__return', ctx=ast.Load()),
                                                   args=[node.value],
                                                   keywords=[]))
        ast.copy_location(new_node, node)
        ast.fix_missing_locations(new_node)
        return new_node

    def visit_For(self, node):
        self.generic_visit(node)
        #| recognize the pattern of PyTorch's DataLoader |#
        def isPyTorchDataLoader(tgt, iter):
            return isinstance(tgt, ast.Tuple) and \
            len(tgt.elts) == 2 and \
            isinstance(tgt.elts[0], ast.Name) and \
            isinstance(tgt.elts[1], ast.Tuple) and \
            len(tgt.elts[1].elts) == 2 and \
            isinstance(tgt.elts[1].elts[0], ast.Name) and \
            isinstance(tgt.elts[1].elts[1], ast.Name) and \
            isinstance(iter, ast.Call) and \
            iter.func.id == 'enumerate' and \
            'loader' in iter.args[0].id

        #| Transforms the target names to list of strings |#
        def targetToList(tgt):
            def extract(x):
                if isinstance(x, ast.Name): return x.id
                elif isinstance(x, ast.Tuple): return targetToList(x.elts)
                else: raise NotImplementedError
            return list(map(extract, tgt))

        def targetToFlatList(tgt):
            res = []
            for item in targetToList(tgt):
                if isinstance(item, list): res.extend(item)
                else: res.append(item)
            return res

        if isPyTorchDataLoader(node.target, node.iter):
            outer_fun_name = self.freshName("forfunc")
            outer_fun = ast.FunctionDef(name=outer_fun_name,
                                        args=ast.arguments(args=list(map(lambda x: ast.arg(arg=x, annotation=None),
                                                                         targetToFlatList(node.target.elts))),
                                                           vararg=None, kwonlyargs=[], kwarg=None, defaults=[], kw_defaults=[]),
                                        body=node.body,
                                        decorator_list=[])
            ast.fix_missing_locations(outer_fun)

            new_node = ast.Expr(ast.Call(func=ast.Name(id='__for_dataloader', ctx=ast.Load()),
                                         args=[ast.Str('DATA_SRC_FILE_FIXME'),
                                               ast.Name(id=outer_fun_name, ctx=ast.Load())],
                                         keywords=[]))
            #ast.copy_location(new_node, node)
            ast.fix_missing_locations(new_node)
            return [outer_fun, new_node]
        else:
            # FIXME target are just names
            new_node = ast.Expr(ast.Call(func=ast.Name(id='__for', ctx=ast.Load()),
                                         args=[node.target, node.iter, node.body],
                                         keywords=[]))
            ast.copy_location(new_node, node)
            ast.fix_missing_locations(new_node)
            return new_node
