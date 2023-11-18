// Test : two definition in different blocks

// GOOD

int main()
{
	int x;
	{
		int x;
	}
	return;
}