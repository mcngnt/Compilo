// Test : fibonacci

// OK

int f(int n)
{
	if(n <= 1)
	{
		return 1;
	}
	return f(n-1) + f(n-2);
}


int main()
{
	int x;
	x = 5;
	return f(x + 3);
}