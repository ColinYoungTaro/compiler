int main() {
  int x = 0;
  {
    int x = 1;
    int y = 2;
  }
  return x + x;
}