$title Codeployment

* 3612 biomass * 3453 plants * 571 county

Sets
       i   bioenergy resource
       j   beccs plant
       m   bebcs plant
       k   basin
       n   CO2 constraints
       ;
$call gdxxrw D:/dx/BECCSBC/S3/abcparameters_simple.xlsx output=D:/dx/BECCSBC/S3/abcparameters_simple1.gdx set=i rng=A2:A3613 rdim=1 set=j rng=E2:E3454 rdim=1 set=m rng=E2:E3454 rdim=1 set=k rng=G2:G572 rdim=1 set=n rng=I2:I6 rdim=1
$GDXIN D:/dx/BECCSBC/S3/abcparameters_simple1.gdx
$load i j m k n
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

$call gdxxrw D:/dx/BECCSBC/S3/abcparameters_simple.xlsx output=D:/dx/BECCSBC/S3/abcparameters_simple2.gdx par=a rng=A2:B3613 rdim=1 par=c rng=G2:H572 rdim=1 par=r_direct rng=C2:D3613 rdim=1 par=d rng=I2:J6 rdim=1 par=area rng=K2:L3613 rdim=1 par=aratio rng=M2:N3613 rdim=1
$GDXIN D:/dx/BECCSBC/S3/abcparameters_simple2.gdx
$load a c r_direct d area aratio
$GDXIN
;


Table d1(i,j)  distance between biomassi and beccs plant j -km;
$call gdxxrw D:/dx/BECCSBC/S3/matrix_biomass_plant.xlsx output=D:/dx/BECCSBC/S3/matrix_biomass_plant.gdx par=d1  rdim=1 cdim=1
$GDXIN D:/dx/BECCSBC/S3/matrix_biomass_plant.gdx
$load d1
$GDXIN
;
Table d2(j,k)  distance between plantsj and basin k -km;;
$call gdxxrw D:/dx/BECCSBC/S3/matrix_plant_county.xlsx output=D:/dx/BECCSBC/S3/matrix_plant_county.gdx par=d2  rdim=1 cdim=1
$GDXIN D:/dx/BECCSBC/S3/matrix_plant_county.gdx
$load d2
$GDXIN
;

Table p2(j,k)  price of pipelines between plantsj and basin k RMB*km-1*t-1;;
$call gdxxrw D:/dx/BECCSBC/S3/matrix_plant_county_price.xlsx output=D:/dx/BECCSBC/S3/matrix_plant_county_price.gdx par=p2  rdim=1 cdim=1
$GDXIN D:/dx/BECCSBC/S3/matrix_plant_county_price.gdx
$load p2
$GDXIN
;
Table d3(i,m)  distance between biomassi and bebcs plant m -km;
$call gdxxrw D:/dx/BECCSBC/S3/matrix_biomass_plant.xlsx output=D:/dx/BECCSBC/S3/matrix_biomass_plant2.gdx par=d3  rdim=1 cdim=1
$GDXIN D:/dx/BECCSBC/S3/matrix_biomass_plant2.gdx
$load d3
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
Scalar TR_ele              biomass to elec MWh per t                            /0.933/ ;
Scalar Au_ele              Auxiliary power ratio                                /0.094/ ;       

*emissions
Scalar E_beccs_lca         LCA emissions tCO2eq per t biomass                   /0/ ;
Scalar E_beccs_cap         carbon captured tCO2eq per t biomass                 /1.485/ ;
Scalar E_beccs_N2O         N2O emissions  tCO2eq per t biomass                  /0/ ;

*Scalar E_beccs_lca         other LCA emissions tCO2eq per t biomass             /-0.339/ ;
*Scalar E_beccs_N2O         N2O emissions  tCO2eq per t biomass                  /0/ ;

*-----------------BCS
*unit cost and benefit,assuming 80000t biomass per year
Scalar C_bcs_inv           Annualized cost of capital RMB                       /2538240/ ;
Scalar C_bcs_lm            Annualized labor and maintain cost RMB               /11898000/ ;
Scalar C_bcs_other         water     fuel 141 RMB per t biomass                 /0/ ;
Scalar C_bcs_appli         biochar application RMB per t biochar                /88.6/ ;
Scalar biopoten2           the potential of biomass accepted t per year         /80000/ ;

Scalar P_oil               oil price RMB per t                                  /0/ ;
Scalar P_gas               gas price RMB per GJ                                 /70.6/ ;
Scalar P_biochar           biochar price RMB per t                              /1925/ ;
Scalar B_yield             crop increase RMB per t yield increase 0.4           /21.05/ ;

*############ type: slow pyrolysis process (to syngas and biochar)
*other parameters
Scalar TR_oil              biomass to oil t per t                               /0/ ;
Scalar TR_gas              biomass to gas GJ per t                              /4.09/ ;
Scalar TR_biochar          biomass to biochar t per t                           /0.286/ ;    

*emissions
Scalar E_bcs_lca           emissions tCO2eq per t biomass                       /0/ ;
Scalar E_bcs_cap           carbon captured tCO2eq per t biomass                 /0.4964/ ;
Scalar E_bcs_N2O           N2O emissions tCO2eq per t biomass                   /0/ ;

*Scalar E_bcs_lca           other emissions tCO2eq per t biomass                 /-0.18/ ;
*Scalar E_bcs_N2O           N2O emissions tCO2eq per t biomass                   /-0.019/ ;

*SOC level
Scalar r_biochar           biochar per hec to maintain current soc level        /2/ ;

Parameter area1(i)  ;   
area1(i)$r_direct(i)=area(i)$((a(i)*aratio(i)/r_direct(i))>area(i))+
                                             (a(i)*aratio(i)/r_direct(i))$((a(i)*aratio(i)/r_direct(i))<area(i)); 



display area1;


Parameter c1(i,j)  transport cost of biomass per t for beccs;
            c1(i,j) = f1 * d1(i,j) ;
Parameter c3(i,m)  transport cost of biomass per t for bcs;
            c3(i,m) = f1 * d3(i,m)  ;
Parameter
          Cos0(n)
          Cos1(n)
          Cos2(n)
          Cos3(n)
          Cos4(n)
          Cos5(n)
          Cos6(n)
          Cos7(n)
          Cos8(n)
          Cos9(n)
          Cos10(n)
          Cos11(n)
          Cos12(n)          
          Cos13(n)
          Cos14(n)
          Cos15(n)
          Cos16(n)
          Cos17(n)
          Cos18(n)
          Cos19(n)
          Cos20(n)
          Cos21(n)
          Cos22(n)
          Cos23(n)
          Cos24(n)
          Cos25(n)
          Cos26(n)
          ;

Variables
       xna(i,j)
       xa(i,j)                 shipment total feedstocks from i to j
       y(j,k)                 shipment quantities from j to k
       wna(i,m)
       wa(i,m)                shipment total feedstocks from i to m
       beccsp(j)              quantities of beccs plants in j
       bcsp(m)                quantities of bcs   plants in m
       z1                     total costs £¤
       ze1                    total costs of beccs use type1
       zc1                    total costs of bcs use type1
       z2 
       ze2
       zc2
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

       z_bcs_har
       z_bcs_biotrans
       z_bcs_biochartrans
       z_bcs_inv
       z_bcs_om
       z_bcs_appli
       b_bcs_ele
       b_bcs_oil
       b_bcs_gas
       b_bcs_yield
       e_bcs1
       e_bcs2
       e_bcs

       realbeccs(j)
       realbcs(m)
       realbeccs1(j)
       realbeccs2(j)
       realbcs1(m)
       realbcs2(m)
       c_grid_cost1(i)
       c_grid_cost2(j)
       c_grid_cost3(i)
       c_grid_cost4(m)
       e_grid_emi1(i)
       e_grid_emi2(i)
       c_grid_cost(i)
       c_grid_beccs(i,j)
       c_grid_bcs(i,m)
       
       d_Tbiotrans1
       d_grid_bio1(i)
       d_grid_bio2(i)
       d_Tcarbtrans
       d_grid_carbon0(j)
       d_grid_carbon(i)                      
       d_Tbiotrans2
       
*       carbonlimit
       ;
Positive Variable xa,xna,y,wa,wna;

Integer Variable beccsp,bcsp;


Equations
       TC1                    define objective function
       T1CBECCS               Type1 
       T1CBCS                 Type1
       TC2   
       T2CBECCS
       T2CBCS
       
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
       

       costbcshar             harvest and processing for bcs
       costbcsbt              transport cost of biomass for bcs
       costbcsbct             transport cost of biochar for bcs
       costbcsinv             investment for bcs
       costbcsom              o and m for bcs
       costbcsappli           biochar application
       oilbcs
       gasbcs
       yield
       capemibcs
       oemiemibcs
       netemibcs
       
       realj(j)
       realm(m)
       realj1(j)
       realj2(j)
       realm1(m)
       realm2(m)
       gridcost1(i) 
       gridcost2(j) 
       gridcost3(i) 
       gridcost4(m) 
       gridemi1(i)
       gridemi2(i)
       supply1(i)              observe supply limit at plantation i
       supply2(i) 
       soc(i)                 to maintain current SOC level
       arrival1(j)
       arrival2(m)
       capture(j)             capture limit at plant j
       injection(k)
       carbon                 carbon limit
       ;

*---------------cost considered only       
       TC1 ..          z1  =e=  ze1+zc1;
       T1CBECCS..      ze1  =e=  z_beccs_har + z_beccs_biotrans + z_beccs_inv + z_beccs_om + z_cap + z_carbtrans + z_stor;
       T1CBCS..        zc1  =e=  z_bcs_har + z_bcs_biotrans + z_bcs_biochartrans + z_bcs_inv + z_bcs_om + z_bcs_appli;
*---------------cost - benefit (coal avoided only for beccs, biochar not considered for bcs)
       TC2 ..          z2  =e=  ze2+zc2;
       T2CBECCS..      ze2  =e=  ze1 - b_beccs_ele - b_beccs_yield;
       T2CBCS..        zc2  =e=  zc1 - b_bcs_oil - b_bcs_gas - b_bcs_yield;    

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

*----------------bcs

* cost 
       costbcshar..    z_bcs_har =e= sum((i,m), C_harvest*(wna(i,m)+wa(i,m)));
       costbcsbt..     z_bcs_biotrans =e= sum((i,m), c3(i,m)*(wna(i,m)+wa(i,m)));
       costbcsbct..    z_bcs_biochartrans =e= sum((i,m), c3(i,m)*TR_biochar*(wna(i,m)+wa(i,m)));
       costbcsinv..    z_bcs_inv =e= sum(m, C_bcs_inv*bcsp(m));
       costbcsom..     z_bcs_om =e= sum(m, C_bcs_lm*bcsp(m))+sum((i,m), C_bcs_other*(wna(i,m)+wa(i,m)));
       costbcsappli..  z_bcs_appli =e= sum((i,m), TR_biochar*(wna(i,m)+wa(i,m))*C_bcs_appli);
* benefit
       oilbcs..        b_bcs_oil =e= sum((i,m), TR_oil*(wna(i,m)+wa(i,m))*P_oil);
       gasbcs..        b_bcs_gas =e= sum((i,m), TR_gas*(wna(i,m)+wa(i,m))*P_gas);
       yield..         b_bcs_yield =e= sum((i,m), (wna(i,m)+wa(i,m))*B_yield);
       
* emission
       capemibcs..     e_bcs1 =e= sum((i,m), E_bcs_cap*(wna(i,m)+wa(i,m)));
       oemiemibcs..    e_bcs2 =e= sum((i,m), (E_bcs_lca+E_bcs_N2O)*(wna(i,m)+wa(i,m)));
       netemibcs..     e_bcs =e= e_bcs1-e_bcs2;
   
*----------------removal costs of each grid
       realj1(j)..      realbeccs1(j)=e=sum(i,xna(i,j));
       realj2(j)..      realbeccs2(j)=e=sum(i,xa(i,j));
       realm1(m)..      realbcs1(m)=e=sum(i,wna(i,m));
       realm2(m)..      realbcs2(m)=e=sum(i,wa(i,m));
       realj(j)..       realbeccs(j)=e=realbeccs1(j)+realbeccs2(j);
       realm(m)..       realbcs(m)=e=realbcs1(m)+realbcs2(m);
       gridcost1(i)..   c_grid_cost1(i)=e=sum(j, C_harvest*(xna(i,j)+xa(i,j))+ c1(i,j)*(xna(i,j)+xa(i,j))+ C_beccs_other*(xna(i,j)+xa(i,j))- TR_ele*(xna(i,j)+xa(i,j))*(1-Au_ele)*P_ele - (xna(i,j)+xa(i,j))*B_yield_beccs );
       gridcost2(j)..   c_grid_cost2(j)=e=C_beccs_inv*beccsp(j)+C_beccs_lm*beccsp(j)+sum(k,(C_capture + p2(j,k)*d2(j,k)+C_storage)*y(j,k));

       gridcost3(i)..   c_grid_cost3(i)=e=sum(m, C_harvest*(wna(i,m)+wa(i,m))+ c3(i,m)*(wna(i,m)+wa(i,m))+ (c3(i,m)+C_bcs_appli)*TR_biochar*(wna(i,m)+wa(i,m)) + C_bcs_other*(wna(i,m)+wa(i,m))- TR_oil*(wna(i,m)+wa(i,m))*P_oil- TR_gas*(wna(i,m)+wa(i,m))*P_gas- (wna(i,m)+wa(i,m))*B_yield);
       gridcost4(m)..   c_grid_cost4(m)=e=C_bcs_inv*bcsp(m)+C_bcs_lm*bcsp(m);
       
       gridemi1(i)..    e_grid_emi1(i)=e=sum(j, (E_beccs_cap*0.99 - E_beccs_lca-E_beccs_N2O)*(xna(i,j)+xa(i,j)));
       gridemi2(i)..    e_grid_emi2(i)=e=sum(m, (E_bcs_cap - E_bcs_lca-E_bcs_N2O)*(wna(i,m)+wa(i,m)));
*----------------constrains
       supply1(i) ..   sum(j, xa(i,j)) + sum(m, wa(i,m))  =l=  a(i)*aratio(i) ;
       supply2(i) ..   sum(j, xna(i,j)) + sum(m, wna(i,m))  =l=  a(i)*(1-aratio(i)) ;
       soc(i)$(area(i)and r_direct(i))..  sum(m, wa(i,m)+wna(i,m))*TR_biochar/r_biochar + (a(i)*aratio(i)-sum(j, xa(i,j))-sum(m, wa(i,m)))/r_direct(i) =g= area1(i);
                     
       arrival1(j) ..  realbeccs(j)  =l=  beccsp(j)* biopoten1 ;
       arrival2(m) ..  realbcs(m)  =l=  bcsp(m)* biopoten2 ;
       capture(j) ..   sum(k, y(j,k))=e= E_beccs_cap*realbeccs(j) ;
       injection(k) .. sum(j, y(j,k))  =l=  c(k)*1000000 ;
       carbon..        e_beccs + e_bcs =g= carbonlimit;



Model transport /all/ ;
Option MIP = CPLEX;
Option resLim=24800;
Option optcr=0.05;
Option SolveLink=0;
$onecho > cplex.opt
threads 3
freegamsmodel  0
cuts -1
$offecho
transport.optfile=1;





LOOP(n,
carbonlimit=d(n);
Solve transport using MIP minimizing z2 ;
*Solve transport using MIP maximizing carbonlimit ;
c_grid_beccs.l(i,j)=(c_grid_cost2.l(j)*(xna.l(i,j)+xa.l(i,j))/realbeccs.l(j))$(realbeccs.l(j)>0);
c_grid_bcs.l(i,m)=(c_grid_cost4.l(m)*(wna.l(i,m)+wa.l(i,m))/realbcs.l(m))$(realbcs.l(m)>0);
c_grid_cost.l(i)=((c_grid_cost1.l(i)+sum(j,c_grid_beccs.l(i,j))+c_grid_cost3.l(i)+sum(m,c_grid_bcs.l(i,m)))/(e_grid_emi1.l(i)+e_grid_emi2.l(i)))$((e_grid_emi1.l(i)+e_grid_emi2.l(i))>0);

*----------------distances
d_Tbiotrans1.l = (sum((i,j), d1(i,j)*(xna.l(i,j)+xa.l(i,j)))/ sum((i,j), (xna.l(i,j)+xa.l(i,j))))$(sum((i,j),(xna.l(i,j)+xa.l(i,j)))>0);
d_grid_bio1.l(i)=(sum(j, d1(i,j)*(xna.l(i,j)+xa.l(i,j)))/sum(j,(xna.l(i,j)+xa.l(i,j))))$(sum(j,(xna.l(i,j)+xa.l(i,j)))>0);
d_Tcarbtrans.l = (sum((j,k), d2(j,k)*y.l(j,k))/sum((j,k), y.l(j,k)))$(sum((j,k),y.l(j,k))>0);
d_grid_carbon0.l(j)=(sum(k, d2(j,k)*y.l(j,k))/sum(k,y.l(j,k)))$(sum(k,y.l(j,k))>0);
d_grid_carbon.l(i)=(sum(j,(xna.l(i,j)+xa.l(i,j))*d_grid_carbon0.l(j))/sum(j,(xna.l(i,j)+xa.l(i,j))))$(sum(j,(xna.l(i,j)+xa.l(i,j)))>0);

d_Tbiotrans2.l = (sum((i,m), d3(i,m)*(wna.l(i,m)+wa.l(i,m)))/ sum((i,m), (wna.l(i,m)+wa.l(i,m))))$(sum((i,m),(wna.l(i,m)+wa.l(i,m)))>0);
d_grid_bio2.l(i)=(sum(m, d3(i,m)*(wna.l(i,m)+wa.l(i,m)))/sum(m,(wna.l(i,m)+wa.l(i,m))))$(sum(m,(wna.l(i,m)+wa.l(i,m)))>0);

Cos0(n)=z2.l;     
Cos1(n)=ze2.l;
Cos2(n)=z_beccs_har.l;     
Cos3(n)=z_beccs_biotrans.l;
Cos4(n)=z_beccs_inv.l;     
Cos5(n)=z_beccs_om.l;      
Cos6(n)=z_cap.l;           
Cos7(n)=z_carbtrans.l;        
Cos8(n)=z_stor.l;          
Cos9(n)=b_beccs_ele.l;          
Cos10(n)=zc2.l;
Cos11(n)=z_bcs_har.l     ;
Cos12(n)=z_bcs_biotrans.l;
Cos13(n)=z_bcs_inv.l     ;
Cos14(n)=z_bcs_om.l      ;
Cos15(n)=b_bcs_oil.l     ;
Cos16(n)=b_bcs_gas.l     ;
Cos17(n)=b_bcs_yield.l   ;
Cos18(n)=z_bcs_biochartrans.l   ;
Cos19(n)=z_bcs_appli.l   ;
Cos20(n)=e_beccs1.l   ;
Cos21(n)=e_beccs2.l   ;
Cos22(n)=e_bcs1.l   ;
Cos23(n)=e_bcs2.l   ;
Cos24(n)=d_Tbiotrans1.l;
Cos25(n)=d_Tcarbtrans.l;
Cos26(n)=d_Tbiotrans2.l;

    if(ord(n) = 1,
       Execute_Unload 'result_xy1.gdx', xa,y,wa,beccsp,bcsp,c_grid_cost,e_bcs,e_beccs,e_grid_emi1,e_grid_emi2,d_grid_bio1,d_grid_carbon,d_grid_bio2,xna,wna;
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = wna Rng =Sheet21!';
       Execute_Unload 'result_z1.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;
    );

        if(ord(n) = 2,
       Execute_Unload 'result_xy2.gdx', xa,y,wa,beccsp,bcsp,c_grid_cost,e_bcs,e_beccs,e_grid_emi1,e_grid_emi2,d_grid_bio1,d_grid_carbon,d_grid_bio2,xna,wna;
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = wna Rng =Sheet21!';
       Execute_Unload 'result_z2.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;
    );

        if(ord(n) = 3,
       Execute_Unload 'result_xy3.gdx', xa,y,wa,beccsp,bcsp,c_grid_cost,e_bcs,e_beccs,e_grid_emi1,e_grid_emi2,d_grid_bio1,d_grid_carbon,d_grid_bio2,xna,wna;
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy3.gdx O = result_xy3.xlsx var = wna Rng =Sheet21!';
       Execute_Unload 'result_z3.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;
    );

        if(ord(n) = 4,
       Execute_Unload 'result_xy4.gdx', xa,y,wa,beccsp,bcsp,c_grid_cost,e_bcs,e_beccs,e_grid_emi1,e_grid_emi2,d_grid_bio1,d_grid_carbon,d_grid_bio2,xna,wna;
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy4.gdx O = result_xy4.xlsx var = wna Rng =Sheet21!';
       Execute_Unload 'result_z4.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;
    );
        if(ord(n) = 5,
       Execute_Unload 'result_xy5.gdx', xa,y,wa,beccsp,bcsp,c_grid_cost,e_bcs,e_beccs,e_grid_emi1,e_grid_emi2,d_grid_bio1,d_grid_carbon,d_grid_bio2,xna,wna;
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = xa Rng =Sheet1!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = y Rng =Sheet2!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = beccsp Rng =Sheet4!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = e_beccs Rng =Sheet8!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = e_grid_emi1 Rng =Sheet9!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = d_grid_bio1 Rng =Sheet15!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = d_grid_carbon Rng =Sheet16!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = xna Rng =Sheet20!';
       Execute 'Gdxxrw  result_xy5.gdx O = result_xy5.xlsx var = wna Rng =Sheet21!';
       Execute_Unload 'result_z5.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;
    );

);
Execute_Unload 'result_z.gdx', Cos0,Cos1,Cos2,Cos3,Cos4,Cos5,Cos6,Cos7,Cos8,Cos9,Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos20,Cos21,Cos22,Cos23,Cos24,Cos25,Cos26;

