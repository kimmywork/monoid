from abc import abstractmethod, ABC
from copy import deepcopy
from typing import List

import numpy as np
from numpy import ndarray


def assert_same_shape(input: ndarray, output: ndarray):
    if input.shape != output.shape:
        raise ValueError(
            f"Input shape {input.shape} does not match output shape {output.shape}"
        )


class Operation(ABC):
    def __init__(self):
        pass

    def forward(self, input_: ndarray):
        self.input_ = input_
        self.output = self._output()
        return self.output

    def backward(self, output_grad: ndarray) -> ndarray:
        assert_same_shape(self.input_, output_grad)

        self.input_grad = self._input_grad(output_grad)
        return self.input_grad

    @abstractmethod
    def _input_grad(self, output_grad: ndarray) -> ndarray:
        pass

    @abstractmethod
    def _output(self):
        pass


class ParamOperation(Operation):
    def __init__(self, param: ndarray):
        super().__init__()
        self.param = param

    def backward(self, output_grad: ndarray) -> ndarray:
        assert_same_shape(self.param, output_grad)

        self.input_grad = self._input_grad(output_grad)
        self.param_grad = self._param_grad(output_grad)

        return self.input_grad

    @abstractmethod
    def _param_grad(self, output_grad: ndarray) -> ndarray:
        pass


class WeightMultiply(ParamOperation):
    def __init__(self, w: ndarray):
        super().__init__(w)

    def _output(self):
        return np.dot(self.input_, self.param)

    def _input_grad(self, output_grad: ndarray) -> ndarray:
        return np.dot(output_grad, np.transpose(self.param, (1, 0)))

    def _param_grad(self, output_grad: ndarray) -> ndarray:
        return np.dot(np.transpose(self.input_, (1, 0)), output_grad)


class BiasAdd(ParamOperation):
    def __init__(self, b: ndarray):
        super().__init__(b)

    def _output(self):
        return np.add(self.input_, self.param)

    def _input_grad(self, output_grad: ndarray) -> ndarray:
        return np.ones_like(self.input_) * output_grad

    def _param_grad(self, output_grad: ndarray) -> ndarray:
        param_grad = np.ones_like(self.param) * output_grad

        return np.sum(param_grad, axis=0).reshape(1, param_grad.shape[1])


class Sigmoid(Operation):
    def __init__(self):
        super().__init__()

    def _output(self):
        return 1.0 / (1.0 + np.exp(-1.0 * self.input_))

    def _input_grad(self, output_grad: ndarray) -> ndarray:
        sigmoid_backward = self.output * (1.0 - self.output)
        input_grad = sigmoid_backward * output_grad

        return input_grad


class Linear(Operation):
    def __init__(self):
        super().__init__()

    def _output(self):
        return self.input_

    def _input_grad(self, output_grad: ndarray) -> ndarray:
        return output_grad


class Layer(ABC):
    def __init__(self, neurons: int):
        self.neurons = neurons
        self.first = True
        self.params: List[ndarray] = []
        self.param_grads: List[ndarray] = []
        self.operations: List[Operation] = []

    @abstractmethod
    def _setup_layer(self, input: ndarray):
        pass

    def forward(self, input_: ndarray) -> ndarray:
        if self.first:
            self._setup_layer(input_)
            self.first = False
        self.input_ = input_

        for operation in self.operations:
            input_ = operation.forward(input_)

        self.output = input_

        return self.output

    def backward(self, output_grad: ndarray) -> ndarray:

        for operation in reversed(self.operations):
            output_grad = operation.backward(output_grad)

        input_grad = output_grad

        self._param_grads()

        return input_grad

    def _param_grads(self):
        self.param_grads = []

        for operation in self.operations:
            if isinstance(operation, ParamOperation):
                self.param_grads.append(operation.param_grad)

    def _params(self):

        self.params = []

        for operation in self.operations:
            if isinstance(operation, ParamOperation):
                self.params.append(operation.param)


class Dense(Layer):
    def __init__(self, neurons: int, activation: Operation = Sigmoid()):
        super().__init__(neurons)
        self.activation = activation

    def _setup_layer(self, input_: ndarray):
        if self.seed:
            np.random.seed(self.seed)

        self.params = []

        self.params.append(np.random.randn(input_.shape[1], self.neurons))

        self.params.append(np.random.randn(1, self.neurons))

        self.operations = [
            WeightMultiply(self.params[0]),
            BiasAdd(self.params[1]),
            self.activation,
        ]


class Loss(ABC):
    def __init__(self):
        pass

    def forward(self, prediction: ndarray, target: ndarray) -> float:
        self.prediction = prediction
        self.target = target

        loss_value = self._output()

        return loss_value

    def backward(self) -> ndarray:
        self.input_grad = self._input_grad()

        return self.input_grad

    @abstractmethod
    def _output(self) -> float:
        pass

    @abstractmethod
    def _input_grad(self) -> ndarray:
        pass


class MeanSquaredError(Loss):
    def __init__(self):
        super().__init__()

    def _output(self) -> float:
        loss = (
            np.sum(np.power(self.prediction - self.target, 2))
            / self.prediction.shape[0]
        )

        return loss

    def _input_grad(self) -> ndarray:

        return 2.0 * (self.prediction - self.target) / self.prediction.shape[0]


class NeuralNetwork:
    def __init__(self, layers: List[Layer], loss: Loss, seed: float = 1):
        self.layers = layers
        self.loss = loss
        self.seed = seed

        if seed:
            for layer in self.layers:
                setattr(layer, "seed", seed)

    def forward(self, x_batch: ndarray) -> ndarray:

        x_out = x_batch

        for layer in self.layers:
            x_out = layer.forward(x_out)

        return x_out

    def backward(self, loss_grad: ndarray):
        grad = loss_grad
        for layer in reversed(self.layers):
            grad = layer.backward(grad)

    def train_batch(self, x_batch: ndarray, y_batch: ndarray) -> float:

        predictions = self.forward(x_batch)

        loss = self.loss.forward(predictions, y_batch)

        self.backward(self.loss.backward())

        return loss

    def params(self):

        for layer in self.layers:
            yield from layer.params

    def param_grads(self):

        for layer in self.layers:
            yield from layer.param_grads


class Optimizer(ABC):
    def __init__(self, lr: float = 0.01):

        self.lr = lr

    @abstractmethod
    def step(self):
        pass


class SGD(Optimizer):
    def __init__(self, lr: float = 0.01):

        super().__init__(lr)

    def step(self):

        for param, param_grad in zip(self.net.params(), self.net.param_grads()):
            param -= self.lr * param_grad


class Trainer(object):
    def __init__(self, net: NeuralNetwork, optim: Optimizer):
        self.net = net
        self.optim = optim
        self.best_loss = 1e9
        setattr(self.optim, "net", self.net)

    def generate_batches(self, X: ndarray, y: ndarray, size: int = 32):

        N = X.shape[0]

        for ii in range(0, N, size):
            X_batch = X[ii : ii + size]
            y_batch = y[ii : ii + size]

            yield X_batch, y_batch

    def fit(
        self,
        X_train: ndarray,
        y_train: ndarray,
        X_test: ndarray,
        y_test: ndarray,
        epochs: int = 100,
        eval_every: int = 10,
        batch_size: int = 32,
        seed: float = 1,
        restart: bool = True,
    ):

        np.random.seed(seed)

        if restart:
            for layer in self.net.layers:
                layer.first = True

        for e in range(epochs):

            if (e + 1) % eval_every == 0:
                last_model = deepcopy(self.net)

            X_train, y_train = permute_data(X_train, y_train)
            batch_generator = self.generate_batches(X_train, y_train, batch_size)

            for ii, (X_batch, y_batch) in enumerate(batch_generator):
                self.net.train_batch(X_batch, y_batch)
                self.optim.step()

            if (e + 1) % eval_every == 0:
                test_preds = self.net.forward(X_test)
                loss = self.net.loss.forward(test_preds, y_test)

                if loss < self.best_loss:
                    self.best_loss = loss
                else:
                    self.net = last_model
                    setattr(self.optim, "net", self.net)
                    break


def permute_data(X, y):
    perm = np.random.permutation(X.shape[0])
    return X[perm], y[perm]
