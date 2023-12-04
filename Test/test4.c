
int print(int* p)
{
	puts(p);
	return 0;
}


int main()
{
	int c;
	int b;
	b = 1;
	while(b)
	{
		print("Password : \n");
		c = getc();
		if(c != 'x')
		{
			puts("Erreur.\n");
		}
		else
		{
			puts("Bravo !\n");
			b = 0;
		}
	}

	return 0;
}