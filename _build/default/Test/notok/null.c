// Test : null type

// WRONG

int* p1;
int** p2;
int*** p3;


int main()
{
	p1 = NULL;
	p2 = NULL + 3;
	p3 = NULL - 1;
	p2 = p1;
	return;
}