p171(lim) = {
tn=0; sf=0; c=0;
for(n0=0, lim, tn=n0; to0=0; 
for(n1=0, lim, to1=tn; tn+=n1; 
	  for(n2=0, lim-tn, to2=tn; tn+=n2; 
	  	    for(n3=0, lim-tn, to3=tn; tn+=n3; 
		    	  for(n4=0, lim-tn, to4=tn; tn+=n4; 
			  	  for(n5=0, lim-tn, to5=tn; tn+=n5; 
				  	  for(n6=0, lim-tn, to6=tn; tn+=n6; 
					  	  for(n7=0, lim-tn, to7=tn; tn+=n7; 
						  	  for(n8=0, lim-tn, to8=tn; tn+=n8; 
							  	  for(n9=0, lim-tn, to9 = tn; tn+=n9; 
								  	  ss = n1 + n2*2^2 + n3*3^2+ n4*4^2 + n5*5^2 + n6*6^2 + n7*7^2 + n8*8^2 + n9*9^2;
									  if (issquare(ss), sf+=ss; print1(n0" "n1" "n2" "n3" "n4" "n5" "n6" "n7" "n8" "n9"\n"););
								  tn=to9);
							tn=to8); 
						tn=to7);
					tn=to6); 
				tn=to5); 
			tn=to4); 
		tn=to3); 
	tn=to2);
tn=to1);
tn=to0)
}

p171(19)

quit