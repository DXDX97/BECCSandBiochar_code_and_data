$title BECCS matching

* 3612 biomass * 3453 plants * 571 county

Sets
       i   bioenergy resource
       j   beccs plant
       k   basin
       n   CO2 constraints
       ;
$call gdxxrw D:/dx/landuse/231129/S1/abcparameters_simple.xlsx output=D:/dx/landuse/231129/S1/abcparameters_simple1.gdx set=i rng=A2:A3613 rdim=1 set=j rng=E2:E3454 rdim=1 set=k rng=G2:G572 rdim=1 set=n rng=I2 rdim=1
$GDXIN D:/dx/landuse/231129/S1/abcparameters_simple1.gdx
$load i j k n
$GDXIN
;

Parameters

       a(i)  
       c(k)  injection of county k (Mt)
       r_direct(i) direct straw return per hec to maintain current soc level (t)
       area(i) cropland area hm2
       aratio(i)  agri of total feedstocks 
       d(n)  CO2 constraint times
       carbonlimit  carbonlimit       
       ;

$call gdxxrw D:/dx/landuse/231129/S1/abcparameters_simple.xlsx output=D:/dx/landuse/231129/S1/abcparameters_simple2.gdx par=a rng=A2:B3613 rdim=1 par=c rng=G2:H572 rdim=1 par=r_direct rng=C2:D3613 rdim=1 par=d rng=I2:J2 rdim=1 par=area rng=K2:L3613 rdim=1 par=aratio rng=M2:N3613 rdim=1
$GDXIN D:/dx/landuse/231129/S1/abcparameters_simple2.gdx
$load a c r_direct d area aratio
$GDXIN
;


Table d1(i,j)  distance between biomassi and beccs plant j -km;
*$call gdxxrw D:/dx/landuse/231129/S1/matrix_biomass_plant.xlsx output=D:/dx/landuse/231129/S1/matrix_biomass_plant.gdx par=d1  rdim=1 cdim=1
$GDXIN D:/dx/landuse/231129/S1/matrix_biomass_plant.gdx
$load d1
$GDXIN
;
Table d2(j,k)  distance between plantsj and basin k -km;;
*$call gdxxrw D:/dx/landuse/231129/S1/matrix_plant_county.xlsx output=D:/dx/landuse/231129/S1/matrix_plant_county.gdx par=d2  rdim=1 cdim=1
$GDXIN D:/dx/landuse/231129/S1/matrix_plant_county.gdx
$load d2
$GDXIN
;

Table p2(j,k)  price of pipelines between plantsj and basin k RMB*km-1*t-1;;
*$call gdxxrw D:/dx/landuse/231129/S1/matrix_plant_county_price.xlsx output=D:/dx/landuse/231129/S1/matrix_plant_county_price.gdx par=p2  rdim=1 cdim=1
$GDXIN D:/dx/landuse/231129/S1/matrix_plant_county_price.gdx
$load p2
$GDXIN
;



*------------------unit costs of BE
Scalar f1                  transport cost of biomass per km t                   /1.3/ ;
Scalar C_harvest           Cost of harvesting the raw material t                /300/ ;



*-----------------BECCS
*unit cost and benefit,assuming  30MW
Scalar C_beccs_inv         Annualized cost of capital RMB                       /24504226.7/ ;
Scalar C_beccs_lm          Annual operating and maintain cost RMB               /16744438.8/ ;
Scalar biopoten1           the potential of biomass accepted t per year         /133583/ ;
*ºóÆÚ²ð¿ª
Scalar C_beccs_other       water     fuel    RMB per t biomass                  /0/ ;
Scalar f2                  transport cost of CO2 per km t                       /0.13/ ;
Scalar C_capture           CO2 capture tCO2                                     /375/ ;
Scalar C_storage           CO2 storage tCO2                                     /50/ ;

Scalar P_ele               bio-electricity price RMB per mwh wo subsity         /500/ ;
Scalar B_yield_beccs       crop increase RMB per t yield increase 0.038         /0/;
*221.75

*other parameters
Scalar TR_ele              biomass to elec MWh per t                            /0.926/ ;
Scalar Au_ele              Auxiliary power ratio                                /0.094/ ;       

*emissions
Scalar E_beccs_lca         LCA emissions tCO2eq per t biomass                   /0/ ;
Scalar E_beccs_cap         carbon captured tCO2eq per t biomass                 /1.485/ ;
Scalar E_beccs_N2O         N2O emissions  tCO2eq per t biomass                  /0/ ;

*Scalar E_beccs_lca         LCA emissions tCO2eq per t biomass                   /0.2274/ ;
*Scalar E_beccs_N2O         N2O emissions  tCO2eq per t biomass                  /0.02/ ;

Parameter area1(i)  ;   
area1(i)$r_direct(i)=area(i)$((a(i)*aratio(i)/r_direct(i))>area(i))+
                                             (a(i)*aratio(i)/r_direct(i))$((a(i)*aratio(i)/r_direct(i))<area(i)); 
display area1;


Parameter c1(i,j)  transport cost of biomass per t for beccs;
            c1(i,j) = f1 * d1(i,j) ;
Parameter
          Cos1(n)
          Cos2(n)
          Cos3(n)
          Cos4(n)
          Cos5(n)
          Cos6(n)
          Cos7(n)
          Cos8(n)
          Cos9(n)
          Cos20(n)
          Cos21(n)
          Cos24(n)
          Cos25(n)
          ;

Variables
       xna(i,j)
       xa(i,j)                shipment total feedstocks from i to j
       y(j,k)                 shipment quantities from j to k
       beccsp(j)              quantities of beccs plants in j
       ze1                    total costs of beccs use type1
       ze2
       z_beccs_har
       z_beccs_biotrans
       z_beccs_inv
       z_beccs_om
       z_cap
       z_carbtrans
       z_stor
       b_beccs_ele
       b_beccs_yield
       e_beccs1
       e_beccs2
       e_beccs


       realbeccs(j)
       realbeccs1(j)
       realbeccs2(j)

       c_grid_cost1(i)
       c_grid_cost2(j)
       e_grid_emi1(i)
       c_grid_cost(i)
       c_grid_beccs(i,j)
       
       d_Tbiotrans1
       d_grid_bio1(i)
       d_grid_bio2(i)
       d_Tcarbtrans
       d_grid_carbon0(j)
       d_grid_carbon(i)                             
*       carbonlimit
       ;
Positive Variable xa,xna,y;

Integer Variable beccsp;


Equations
       T1CBECCS               Type1 
       T2CBECCS
       
       costbeccshar           harvest and processing for beccs
       costbeccsbt            transport cost of biomass for beccs
       costbeccsinv           investment for beccs
       costbeccsom            o and m for beccs
       costcap                cost of CO2 capture
       costcarbtrans          transport cost of CO2
       coststor               cost of CO2 storage
       elebeccs
       yieldbeccs
       capbeccs 
       oemibeccs
       netemibeccs
       
       
       realj(j)
       realj1(j)
       realj2(j)
       gridcost1(i) 
       gridcost2(j) 
       gridemi1(i)
       supply1(i)              observe supply limit at plantation i
       supply2(i) 
       soc(i)                 to maintain current SOC level
       arrival1(j)
       capture(j)             capture limit at plant j
*       injection(k)
       carbon                 carbon limit
       ;

*---------------cost considered only       
       T1CBECCS..      ze1  =e=  z_beccs_har + z_beccs_biotrans + z_beccs_inv + z_beccs_om + z_cap + z_carbtrans + z_stor;

*---------------cost - benefit (coal avoided only for beccs, biochar not considered for bcs)
       T2CBECCS..      ze2  =e=  ze1 - b_beccs_ele - b_beccs_yield;


*---------------beccs

* cost  
       costbeccshar..  z_beccs_har =e= sum((i,j), C_harvest*(xna(i,j)+xa(i,j)));
       costbeccsbt..   z_beccs_biotrans =e= sum((i,j), c1(i,j)*(xna(i,j)+xa(i,j)));
       costbeccsinv..  z_beccs_inv =e= sum(j, C_beccs_inv*beccsp(j));
       costbeccsom..   z_beccs_om =e= sum(j, C_beccs_lm*beccsp(j))+sum((i,j), C_beccs_other*(xna(i,j)+xa(i,j)));
       costcap..       z_cap =e= sum((j,k), C_capture*y(j,k));
       costcarbtrans.. z_carbtrans =e= sum((j,k), p2(j,k)*d2(j,k)*y(j,k));
       coststor..      z_stor =e= sum((j,k), C_storage*y(j,k));
* benefit
       elebeccs..      b_beccs_ele =e= sum((i,j), TR_ele*(xna(i,j)+xa(i,j))*(1-Au_ele)*P_ele);
       yieldbeccs..    b_beccs_yield =e= sum((i,j), (xna(i,j)+xa(i,j))*B_yield_beccs);
* emission
       capbeccs..      e_beccs1 =e= sum((i,j), E_beccs_cap*(xna(i,j)+xa(i,j)));
       oemibeccs..     e_beccs2 =e= sum((i,j), (E_beccs_lca+E_beccs_N2O)*(xna(i,j)+xa(i,j)));
       netemibeccs..   e_beccs =e= sum((j,k),y(j,k))*0.99- e_beccs2;
   
*----------------removal costs of each grid
       realj1(j)..      realbeccs1(j)=e=sum(i,xna(i,j));
       realj2(j)..      realbeccs2(j)=e=sum(i,xa(i,j));
       realj(j)..       realbeccs(j)=e=realbeccs1(j)+realbeccs2(j);
       gridcost1(i)..   c_grid_cost1(i)=e=sum(j, C_harvest*(xna(i,j)+xa(i,j))+ c1(i,j)*(xna(i,j)+xa(i,j))+ C_beccs_other*(xna(i,j)+xa(i,j))- TR_ele*(xna(i,j)+xa(i,j))*(1-Au_ele)*P_ele - (xna(i,j)+xa(i,j))*B_yield_beccs );
       gridcost2(j)..   c_grid_cost2(j)=e=C_beccs_inv*beccsp(j)+C_beccs_lm*beccsp(j)+sum(k,(C_capture + p2(j,k)*d2(j,k)+C_storage)*y(j,k));
       
       gridemi1(i)..    e_grid_emi1(i)=e=sum(j, (E_beccs_cap - E_beccs_lca-E_beccs_N2O)*(xna(i,j)+xa(i,j)));
*----------------constrains
       supply1(i) ..   sum(j, xa(i,j))  =l=  a(i)*aratio(i) ;
       supply2(i) ..   sum(j, xna(i,j)) =l=  a(i)*(1-aratio(i)) ;
       soc(i)$(area(i)and r_direct(i))..  (a(i)*aratio(i)-sum(j, xa(i,j)))/r_direct(i) =g= area1(i);
                     
       arrival1(j) ..  realbeccs(j)  =l=  beccsp(j)* biopoten1 ;
       capture(j) ..   sum(k, y(j,k))=e= E_beccs_cap*realbeccs(j) ;
*       injection(k) .. sum(j, y(j,k))  =l=  c(k)*1000000 ;
       carbon..        e_beccs =g= carbonlimit;



Model transport /all/ ;
Option MIP = CPLEX;
Option resLim=17200;
Option optcr=0.05;
Option SolveLink=0;
$onecho > cplex.opt
threads 4
freegamsmodel  0
cuts -1
$offecho
transport.optfile=1;





LOOP(n,
carbonlimit=d(n);
Solve transport using MIP minimizing ze2 ;
*Solve transport using MIP maximizing carbonlimit ;
c_grid_beccs.l(i,j)=(c_grid_cost2.l(j)*(xna.l(i,j)+xa.l(i,j))/realbeccs.l(j))$(realbeccs.l(j)>0);
c_grid_cost.l(i)=((c_grid_cost1.l(i)+sum(j,c_grid_beccs.l(i,j)))/(e_grid_emi1.l(i)))$(e_grid_emi1.l(i)>0);

*----------------distances
d_Tbiotrans1.l = (sum((i,j), d1(i,j)*(xna.l(i,j)+xa.l(i,j)))/ sum((i,j), (xna.l(i,j)+xa.l(i,j))))$(sum((i,j),(xna.l(i,j)+xa.l(i,j)))>0);
d_grid_bio1.l(i)=(sum(j, d1(i,j)*(xna.l(i,j)+xa.l(i,j)))/sum(j,(xna.l(i,j)+xa.l(i,j))))$(sum(j,(xna.l(i,j)+xa.l(i,j)))>0);
d_Tcarbtrans.l = (sum((j,k), d2(j,k)*y.l(j,k))/sum((j,k), y.l(j,k)))$(sum((j,k),y.l(j,k))>0);
d_grid_carbon0.l(j)=(sum(k, d2(j,k)*y.l(j,k))/sum(k,y.l(j,k)))$(sum(k,y.l(j,k))>0);
d_grid_carbon.l(i)=(sum(j,(xna.l(i,j)+xa.l(i,j))*d_grid_carbon0.l(j))/sum(j,(xna.l(i,j)+xa.l(i,j))))$(sum(j,(xna.l(i,j)+xa.l(i,j)))>0);
   
Cos1(n)=ze2.l;
Cos2(n)=z_beccs_har.l;     
Cos3(n)=z_beccs_biotrans.l;
Cos4(n)=z_beccs_inv.l;     
Cos5(n)=z_beccs_om.l;      
Cos6(n)=z_cap.l;           
Cos7(n)=z_carbtrans.l;        
Cos8(n)=z_stor.l;          
Cos9(n)=b_beccs_ele.l;          
Cos20(n)=e_beccs1.l   ;
Cos21(n)=e_beccs2.l   ;
Cos24(n)=d_Tbiotrans1.l;
Cos25(n)=d_Tcarbtrans.l;

    if(ord(n) = 1,
       Execute_Unload 'result_xy1.gdx', xa,y,beccsp,c_grid_cost,e_beccs,e_grid_emi1,d_grid_bio1,d_grid_carbon,xna,area1;
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = area1 Rng =Sheet22!';
       Execute_Unload 'result_z1.gdx', Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos20,Cos21,Cos24,Cos25;
    );

    if(ord(n) = 2,
       Execute_Unload 'result_xy2.gdx', xa,y,beccsp,c_grid_cost,e_beccs,e_grid_emi1,d_grid_bio1,d_grid_carbon,xna,area1;
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = area1 Rng =Sheet22!';
       Execute_Unload 'result_z2.gdx', Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos20,Cos21,Cos24,Cos25;
    );

    if(ord(n) = 3,
       Execute_Unload 'result_xy3.gdx', xa,y,beccsp,c_grid_cost,e_beccs,e_grid_emi1,d_grid_bio1,d_grid_carbon,xna,area1;
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = area1 Rng =Sheet22!';
       Execute_Unload 'result_z3.gdx', Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos20,Cos21,Cos24,Cos25;
    );

    if(ord(n) = 4,
       Execute_Unload 'result_xy4.gdx', xa,y,beccsp,c_grid_cost,e_beccs,e_grid_emi1,d_grid_bio1,d_grid_carbon,xna,area1;
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = area1 Rng =Sheet22!';
       Execute_Unload 'result_z4.gdx', Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos20,Cos21,Cos24,Cos25;
    );
);
Execute_Unload 'result_z.gdx', Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos20,Cos21,Cos24,Cos25;


