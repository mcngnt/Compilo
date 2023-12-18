// Test : fibonacci

// OK

int res;

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


int f(int n, int a)
{
	if(n <= 1)
	{
		return 1;
	}
	return f(n-1, a+1) + f(n-2, a+2);
}


int main()
{
	res = f(11,0);
	print_int_nl(res);
	putc('\n');
	return res;
}