// Test : compute square root

// OK

int print_int(int t)
{
    if(t <= 9)
    {
        putc('0' + t);
        return;
    }
    print_int(t/10);
    putc('0' + (t % 10));
    return;
}

int print_int_nl(int t)
{
    if(t < 0)
    {
        t = -t;
        putc('-');
    }
    print_int(t);
    putc('\n');
    return;
}

int sq(int n)
{
	int k;
	k = 0;
	while(k*k <= n)
	{
		k++;
	}
	return k-1;
}


int main()
{
	print_int_nl(sq(99));
	return;
}