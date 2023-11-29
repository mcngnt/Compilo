int glob;

int main()
{
	int x;
	int y;
	x = 3;
	y = 2;
	while(x > 0)
	{
		x = x - 1;
		y = y * y;
	}
	glob = y;
	return y;
}