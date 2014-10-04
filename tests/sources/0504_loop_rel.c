int N = rand(0,1000);
int x = 0;
while (x < N) {
  print(x,N);
  x = x + 1;
}
assert(x==N);
