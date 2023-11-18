// Test : wrong function call

// WRONG

int f(int* y)
{
	return *y;
}

int main()
{
	int a;
	a = 17;
	return f(a);
}