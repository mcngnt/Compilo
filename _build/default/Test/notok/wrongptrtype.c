// Test : incomptible pointer types

// WRONG


int main()
{
	int* p;
	int** q;
	int a;
	a = 10;
	p = &a;
	q = &p;
	return p + q;
}