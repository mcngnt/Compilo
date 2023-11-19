// Test : compute square root

// OK

int sq(int n)
{
	int k;
	k = 0;
	while(k*k <= n)
	{
		k++;
	}
	return k;
}


int main()
{
	return sq(100);
}