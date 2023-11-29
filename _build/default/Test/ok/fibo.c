// Test : fibonacci

// OK

int f(int n, int a)
{
	if(n <= 1)
	{
		return 1;
	}
	return f(n-1, a+1) + f(n-2, a+2);
}


int main()
{
	return f(5,0);
}