	program read_DOSCAR
	implicit none
	integer,parameter :: nn=50,nat=16

	real :: var1_tot, var2_tot, var3_tot, var4_tot, &
		var5_tot,E_tot(1:301),DOS_tot(1:301,1:3), &
		DOS(1:196,1:301,1:10),dos_s(nn,1:301), &
		dos_p(nn,1:301), dos_d(nn,1:301), dos_ss(301), &
		dos_pp(301), dos_dd(301),tmp1,tmp2,tmp3,tmp_tot,&
                p1(301),p2(301),p3(301),d1(301),d2(301),&
                d3(301),d4(301),d5(301),s(301)



	integer :: natoms, ilines, jlines, j


        integer :: &
        naa(nat)=(/5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80/)


	open(3,file='DOSCAR')
	read(3,104) natoms
 104	format(1X,i3) 

	do 10 ilines = 1,4
	 read(3,*)
 10	continue

	read(3,*) var1_tot, var2_tot, var3_tot, var4_tot, var5_tot
	do 11 ilines = 1,301
	read(3,105) (DOS_tot(ilines,j),j=1,3)
 105	format(f11.3,2E12.4E2)
 11	continue

	do 12 ilines = 1,natoms
	read(3,*)
	do 13 jlines = 1,301
	read(3,107)(DOS(ilines,jlines,j),j=1,10)
 107	format(f11.3,9E12.4E2)
 13	continue
 12	continue

	do 15 ilines= 1,nat
	do 16 jlines = 1,301
	dos_s(naa(ilines),jlines)= DOS(naa(ilines),jlines,2)
	dos_p(naa(ilines),jlines)= DOS(naa(ilines),jlines,3)+ &
	DOS(naa(ilines),jlines,4)+DOS(naa(ilines),jlines,5)

	dos_d(naa(ilines),jlines)= DOS(naa(ilines),jlines,6) + &
				   DOS(naa(ilines),jlines,7) + &
				   DOS(naa(ilines),jlines,8) + &
				   DOS(naa(ilines),jlines,9) + &
				   DOS(naa(ilines),jlines,10)

 16	continue
 15	continue
	do 19 jlines = 1,301
	dos_ss(jlines)= 0.0000E+00
	dos_pp(jlines)= 0.0000E+00
	dos_dd(jlines)= 0.0000E+00
        s(jlines)= 0.0000E+00
        p1(jlines)= 0.0000E+00
        p2(jlines)= 0.0000E+00
        p3(jlines)= 0.0000E+00
        d1(jlines)= 0.0000E+00
        d2(jlines)= 0.0000E+00
        d3(jlines)= 0.0000E+00
        d4(jlines)= 0.0000E+00
        d5(jlines)= 0.0000E+00
 19	continue
	do 17 ilines= 1,nat
	do 18 jlines = 1,301
	dos_ss(jlines)= dos_ss(jlines) + dos_s(naa(ilines),jlines)
	dos_pp(jlines)= dos_pp(jlines) + dos_p(naa(ilines),jlines)
	dos_dd(jlines)= dos_dd(jlines) + dos_d(naa(ilines),jlines)
        s(jlines) = s(jlines)  + DOS(naa(ilines),jlines,2)
        p1(jlines)= p1(jlines) + DOS(naa(ilines),jlines,3)
        p2(jlines)= p2(jlines) + DOS(naa(ilines),jlines,4)
        p3(jlines)= p3(jlines) + DOS(naa(ilines),jlines,5)
        d1(jlines)= d1(jlines) + DOS(naa(ilines),jlines,6)
        d2(jlines)= d2(jlines) + DOS(naa(ilines),jlines,7)
        d3(jlines)= d3(jlines) + DOS(naa(ilines),jlines,8)
        d4(jlines)= d4(jlines) + DOS(naa(ilines),jlines,9)
        d5(jlines)= d5(jlines) + DOS(naa(ilines),jlines,10)
 18	continue
 17	continue

	open(UNIT=2,FILE= "DOS_tot" , ACTION="write",STATUS= "replace")
	do 91 jlines = 1,301
 	write(2,106) DOS(1,jlines,1)-var4_tot, dos_ss(jlines),&
                               dos_pp(jlines), dos_dd(jlines),&
                     s(jlines),p1(jlines),p2(jlines),p3(jlines),&
                     d1(jlines),d2(jlines),d3(jlines),d4(jlines),&
                    d5(jlines)
 106    format(f11.3,' ',E12.4E2,' ',E12.4E2,' ',E12.4E2,' ',E12.4E2,&
               ' ',E12.4E2,' ',E12.4E2,' ',E12.4E2,' ',E12.4E2,' ',&
               E12.4E2,' ',E12.4E2,' ',E12.4E2,' ',E12.4E2)

 91	continue
	tmp3 = 0
	tmp_tot = 0
	do 92 jlines= 1,301
	tmp1 = DOS(1,jlines,1)-var4_tot
	tmp3 = tmp3 + dos_dd(jlines)
	tmp2 = tmp1 * dos_dd(jlines)
	tmp_tot = tmp_tot + tmp2
 92 	continue
	write(2,'(A)') " "
	write(2,'(A)') " "
	write(2,111) tmp_tot/tmp3 , 100
 111	format(E12.4E2,I4.3)



	return
	end program
