
#include <stdio.h>

#include <caml/memory.h>


value caml_foreign_hello(value unit) {
    CAMLparam1(unit);

    printf("Hello world!");

    CAMLreturn(Val_unit);
}
