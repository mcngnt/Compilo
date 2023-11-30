int glob;

int main()
{
	int* p;
	int x;
	int y;
	x = -3;
	y = 1 ? 5 : 0;
	p = &y;
	glob = *p;
	return 0;
}

