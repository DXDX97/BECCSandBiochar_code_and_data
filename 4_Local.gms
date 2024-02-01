$title Local

Sets
       i     bioenergy resource
       in(i) grid on the basin
       k     county  1-349
       ;
*only consider susforest, susCrop, and susMan
*$call gdxxrw D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan.xlsx output=D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan1.gdx set=i rng=A2:A3613 rdim=1 set=in rng=F2:F1155 rdim=1  set=k rng=H2:H349 rdim=1
*$GDXIN D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan1.gdx


*consider all
$call gdxxrw D:/dx/landuse/231129/Local/abcparameters_simple.xlsx output=D:/dx/landuse/231129/Local/abcparameters_simple1.gdx set=i rng=A2:A3613 rdim=1 set=in rng=F2:F1164 rdim=1  set=k rng=H2:H349 rdim=1
$GDXIN D:/dx/landuse/231129/Local/abcparameters_simple1.gdx
$load i in k
$GDXIN
;

Parameters

       a(i)  
       r_direct(i) direct straw return per hec to maintain current soc level (t)
       area(i) cropland area hm2
       aratio(i)  agri of total feedstocks
       countyinj(k)
       pac(in)
       pac2(k)
       ;

*only consider susforest and susCrop
*$call gdxxrw D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan.xlsx output=D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan2.gdx par=a rng=A2:B3613 rdim=1 par=r_direct rng=C2:D3613 rdim=1 par=area rng=L2:M3613 rdim=1 par=aratio rng=N2:O3613 rdim=1 par=countyinj rng=J2:K349 rdim=1 par=pac rng=F2:G1155 rdim=1 par=pac2 rng=H2:I349 rdim=1
*$GDXIN D:/dx/landuse/231129/Local/abcparameters_simple_ForCropMan2.gdx

*consider all
$call gdxxrw D:/dx/landuse/231129/Local/abcparameters_simple.xlsx output=D:/dx/landuse/231129/Local/abcparameters_simple2.gdx par=a rng=A2:B3613 rdim=1 par=r_direct rng=C2:D3613 rdim=1 par=area rng=L2:M3613 rdim=1 par=aratio rng=N2:O3613 rdim=1 par=countyinj rng=J2:K349 rdim=1 par=pac rng=F2:G1164 rdim=1 par=pac2 rng=H2:I349 rdim=1
$GDXIN D:/dx/landuse/231129/Local/abcparameters_simple2.gdx
$load a r_direct area aratio countyinj pac pac2
$GDXIN
; 
Parameters
     tw
     tx
     ;

*emissions
Scalar E_beccs_lca         LCA emissions tCO2eq per t biomass                   /0/ ;
Scalar E_beccs_cap         carbon captured tCO2eq per t biomass                 /1.47/ ; 
Scalar E_beccs_N2O         N2O emissions  tCO2eq per t biomass                  /0/ ;     

*emissions
Scalar E_bcs_lca           emissions tCO2eq per t biomass                       /0/;
Scalar E_bcs_cap           carbon captured tCO2eq per t biomass                 /0.3123/ ;
Scalar E_bcs_N2O           N2O emissions tCO2eq per t biomass                   /0/ ;
Scalar TR_biochar                              /0.174/ ;
*SOC level
Scalar r_biochar           biochar per hec to maintain current soc level        /2/ ;

Parameter area1(i)  ;   
area1(i)$r_direct(i)=area(i)$((a(i)*aratio(i)/r_direct(i))>area(i))+
                                             (a(i)*aratio(i)/r_direct(i))$((a(i)*aratio(i)/r_direct(i))<area(i)); 

display area1;

Variables
       xna(i)               non-agri for beccs in grid i
       wna(i)               non-agri for biochar in grid i
       xa(i)                agri for beccs in grid i 
       wa(i)                agri for biochar in grid i
       carbonlimit
       inj(in)
       ;
Positive Variable
          xa
          wa
          xna
          wna
          inj
          ;



Equations
       supply(i)              observe supply limit at plantation i
       supply2(i)
       soc(i)                 to maintain current SOC level
       injection
*       injection(in)
*       injection2(i)
*       inlimit(k)
       carbon                 carbon limit
       ;
*----------------constrains
       supply(i) ..   xna(i)+wna(i)  =l=  a(i)*(1-aratio(i)) ;
       supply2(i) ..   xa(i) + wa(i)  =l=  a(i)*aratio(i) ;
*       soc(i)$(area(i)and r_direct(i))..  wa(i)*TR_biochar/r_biochar + (a(i)*aratio(i)-xa(i)-wa(i))/r_direct(i) =g= area1(i);                                                              
       soc(i)$(area(i)and r_direct(i))..  (wa(i)+wna(i))*TR_biochar/r_biochar + (a(i)*aratio(i)-xa(i)-wa(i))/r_direct(i) =g= area1(i);                                                              

*       injection(in) .. (xna(in)+xa(in))*E_beccs_cap  =l=  inj(in)*1000000 ;
*       injection2(i)$(not in(i)) .. (xna(i)+xa(i))*E_beccs_cap  =l=  0 ;
*       inlimit(k)..     sum(in$(pac(in)=pac2(k)),inj(in))=l=countyinj(k) ;
       injection..       sum(i,(xna(i)+xa(i))*E_beccs_cap/0.99)  =l=  2.29703922252741*1000000000 ;
       carbon..        sum(i, (xna(i)+xa(i))*(E_beccs_cap-E_beccs_lca-E_beccs_N2O) + (wna(i)+wa(i))*(E_bcs_cap-E_bcs_lca-E_bcs_N2O)) =g= carbonlimit;



Model transport /all/ ;
Option LP = CPLEX;
Option resLim=100000;
Option optcr=0.01;
Option SolveLink=0;
$onecho > cplex.opt
threads 3
freegamsmodel  0
cuts -1
$offecho
transport.optfile=1;

Solve transport using LP maximizing carbonlimit ;
tw=sum(i,wna.l(i)+wa.l(i));
tx=sum(i,xna.l(i)+xa.l(i));
display tw,tx;

