int glob;

int f(int x)
{
  return 2 * x;
}

int main() 
{
  int a;
  int* p;
  p = NULL;
  glob = 3+2;
  {
      int p;
      p = 36;
      a = p;
  }
  a = f(glob + 5);
  return 0;
}

