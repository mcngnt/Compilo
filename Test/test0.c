int glob;

int main()
{
	int y;
	int z;
	int* p;
	int x;
	int w;

	x = 5;
	p = &x;
	++(*p);
	glob = --(*p);

	return 0;
}

