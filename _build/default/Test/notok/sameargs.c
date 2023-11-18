// Test : same name in args

// WRONG

int f(int x, int x)
{
	return x;
}


int main()
{
	return f(1,2);
}