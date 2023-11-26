
int global;

int main()
{
	int y;
	int x;
	y = 2;
	x = 0;
	while(y > 0)
	{
		x = x + 2;
		y = y - 1;
	}
	global = x;
	return x;
}