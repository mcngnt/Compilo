int res;

int f(int a, int b)
{
	if(a == 1)
	{
		return b;
	}
	if(b == 1)
	{
		return a;
	}
	return a + b - 1 + f(a-1,b-1);
}

int main()
{
	res = f(2,500);
	return res;
}


