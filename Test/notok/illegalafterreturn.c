// Test : illegal affectation anfter a return

// WRONG


int main()
{
	int x;
	x = 0;
	if(x)
	{
		return x;
	}
	else
	{
		x = NULL;
	}
	return 0;
}