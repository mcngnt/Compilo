int res;

int f(int n)
{
	if(n == 1)
	{
		return 1;
	}
	return 1 + f(n-1);
}

int main()
{
	res = f(2);
	return;
}