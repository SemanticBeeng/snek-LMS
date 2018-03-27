# from pylms import lms, lmscompile, stage, ast
from .rep import *
import torch

__all__ = ['nn_linear', 'RepTensor', 'optim_SGD']

stFresh = 0

def freshTensor(*dims):
    global stFresh
    stFresh += 1
    return RepTensor("t"+str(stFresh-1), *dims)

class RepTensor(Rep):
    def __init__(self, n, *dims):
        super().__init__(n)
        self.dims = [i for i in dims]
    def __mul__(self, m):
        return reflect(["dot",self,m])
    def __repr__(self):
        return "[tensor, [{}]]".format(", ".join(list(map(str, self.dims))))

def nn_linear(hlsize, outsize):
    class RepLinear(object):
        def __init__(self):
            self.weight = reflect(freshTensor(outsize, hlsize))
            self.bias = reflect(freshTensor(outsize))
            self.linear = None

        def __call__(self, tensor):
            if isinstance(tensor, torch.Tensor): #unstaged
                if self.linear is None:
                    self.linear = nn.Linear(hlsize, outsize)

                return self.linear(tensor)
            else: #staged
                return self.weight * tensor + self.bias

    return RepLinear()

def optim_SGD(params, lr, momentum):
    class RepSGD(Rep):
        def __init__(self, n):
            super().__init__(n)
            if isinstance(params, list):
                self.staged = False
                self.optim = optim.SGD(params, lr, momentum)
            else:
                self.staged = True
                self.optim = reflect([self, [lr, momentum]])

        def zero_grad(self):
            if self.staged:
                return reflect(["call", self, "zero_grad"])
            else:
                return self.optim.zero_grad()

        # def __repr__(self):
        #     return

    return RepSGD("SGD")

# def F_nll_loss(output, target, size_average):