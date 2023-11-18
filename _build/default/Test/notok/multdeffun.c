// Test : two definition for the same function

// WRONG

int f(int x)
{
	return x;
}

int f(int z)
{
	return 2 * z;
}

int main()
{
	int x;
	int x;
	return;
}