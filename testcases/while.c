int main() {
  int x = 10;
  while(x > 0) {
    x = x - 1;
    if(x == 3) {
      return x;
    }
  }
  return x;
}