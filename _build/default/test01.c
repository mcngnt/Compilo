int glob;

int f(int x)
{
  int* a;
  a = NULL;
  x = x * 2;
  if(x == 2)
  {
    x = 3;
  }
  else
  {
    return x;
  }
  return x;
}

int* g(int** p)
{
  return *p;
}


int main() 
{
  int a;
  int** p;
  p = NULL;
  glob = 3+2;
  {
      int p;
      p = 36;
      a = p + 2;
      while(~(a == p))
      {
        a = a + 1;
      }
  }
  a = f(glob + 5);
  return 0;
}

