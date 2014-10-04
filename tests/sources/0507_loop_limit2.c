int x = 0;
int N = rand(0,1000);
while (rand(0,1)==0) {
  if (x < N) x = x + 1;
}
print(x);
