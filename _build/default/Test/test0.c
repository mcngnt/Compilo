
int x;
int y;
int z;
int* p;

int main()
{
	p = NULL;
	x = -(0 - 5);
	{
		int w;
		w = 7;
	}
	y = x > 0 ? 3 : -100/2;
	z = x * y;
	if(z < y)
	{
		while(z > y)
		{
			--z;
		}
	}
	else
	{
		p = &y;
		++p;
		x = *p;
		return 0;
		x = ~666;
	}
	return 0;		
}