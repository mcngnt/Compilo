
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

int f()
{
	int a;
	int b;
	for (a = -100; a < 100; a++)
	{
	    for (b = -100; b < 100; b++)
	    {
	        if (b != 0 && (a/b)*b + a%b != a)
	        {
	            return -1;
	        }
	    }
	}
	return 0;
}


int main()
{
	int res;
	res = f();
	print_int_nl(res);
    return;
}

