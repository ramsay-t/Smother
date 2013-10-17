-record(bool_log,
	{exp,
	 tcount=0,
	 fcount=0,
	 tsubs=[],
	 fsubs=[]
	}).

-record(pat_log,
	{exp,
	 guards=[],
	 mcount=0,
	 nmcount=0,
	 subs=[],
	 extras=[],
	 matchedsubs=[]
	}).
