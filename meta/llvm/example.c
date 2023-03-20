

int printf(const char*, ...);

__attribute__((noinline))
static int identity(int i) {
  return i;
}

void print_int(int i) {
  printf("%d", identity(i));
}
