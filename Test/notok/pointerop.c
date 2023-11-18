// Test : illegal pointer operation

// WRONG

int main()
{
	int* p1;
	int* p2;
	int a;
	a = 45;
	p1 = &a;
	p2 = &a;

	return p1 + p2;
}