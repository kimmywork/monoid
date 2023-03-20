from dataclasses import dataclass, field


@dataclass
class Name:
    first: str
    last: str


@dataclass
class Employee:
    name: Name
    age: int
    salary: float = field(default=200.)


def get_name(employee):
    match employee:
        # pylint: disable=used-before-assignment
        case Employee(
            Name(first, last),
            _,
            salary=float(salary)
        ) if salary > 10:
            return f'{first} {last}', salary
        case _:
            return None


def head(lst: list):
    match lst:
        case [element, *_]:
            return element
        case _:
            return None


def tail(lst: list):
    match lst:
        case [_, *rest]:
            return rest
        case _:
            return []


print(get_name(Employee(**dict(name=Name(first='Kimmy', last='Liu'), age=20))))
