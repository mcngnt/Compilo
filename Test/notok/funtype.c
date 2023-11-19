// Test : function return type doesn't fit variable type

// WRONG

int* f(int x)
{
	return NULL + x;
}

int main()
{
	int x;
	x = f(8);
}
