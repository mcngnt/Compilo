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
  a = f(glob + 5 + NULL);
  return 0;
}

