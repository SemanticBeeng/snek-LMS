#/usr/bin/python3

#TODO: Add proper imports

class SchemeGenIRDef(object):
    def gen(self, irdef):
        bodycode = SchemeCodeGen(irdef.body).gen()
        return "(define ({0} {1}) {2})".format(irdef.name, " ".join(irdef.args), bodycode)

class SchemeGenIRConst(object):
    def gen(self, irconst): return str(irconst.v)

class SchemeGenIRInt(object):
    def gen(self, irint): return str(irint.n)

class SchemeGenIRIntAdd(object):
    def gen(self, iradd):
        lhscode = SchemeCodeGen(iradd.lhs).gen()
        rhscode = SchemeCodeGen(iradd.rhs).gen()
        return "(+ {0} {1})".format(lhscode, rhscode)

class SchemeGenIRIntMul(object):
    def gen(self, irmul):
        lhscode = SchemeCodeGen(irmul.lhs).gen()
        rhscode = SchemeCodeGen(irmul.rhs).gen()
        return "(* {0} {1})".format(lhscode, rhscode)

class SchemeCodeGen(CodeGen):
    def __init__(self, ir):
        self.ir = ir

    def gen(self):
        clsName = "SchemeGen{0}".format(type(self.ir).__name__)
        Cls = getClass(clsName)
        return Cls().gen(self.ir)

