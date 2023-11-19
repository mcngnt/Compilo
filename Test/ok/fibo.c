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
	return f(5);
}