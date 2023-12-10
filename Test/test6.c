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
    print_int(t);
    putc('\n');
    return;
}

int f(int a, int b)
{
	if(a == 1)
	{
		return b;
	}
	if(b == 1)
	{
		return a;
	}
	return a + b - 1 + f(a-1,b-1);
}

int main()
{
	res = f(2,500);
	print_int_nl(res);
	return res;
}


