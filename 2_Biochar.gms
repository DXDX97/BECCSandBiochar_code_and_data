$title Biochar



Sets
       i   bioenergy resource
       m   bebcs plant
       n   CO2 constraints
       ;
$call gdxxrw D:/dx/BECCSBC/S2/abcparameters_simple.xlsx output=D:/dx/BECCSBC/S2/abcparameters_simple1.gdx set=i rng=A2:A3613 rdim=1 set=m rng=E2:E3454 rdim=1 set=n rng=I2:I3 rdim=1
$GDXIN D:/dx/BECCSBC/S2/abcparameters_simple1.gdx
$load i m n
$GDXIN
;

Parameters

       a(i)  
       r_direct(i) direct straw return per hec to maintain current soc level (t)
       area(i) cropland area hm2
       aratio(i)  agri of total feedstocks 
       d(n)  CO2 constraint times
       carbonlimit  carbonlimit       
       ;

$call gdxxrw D:/dx/BECCSBC/S2/abcparameters_simple.xlsx output=D:/dx/BECCSBC/S2/abcparameters_simple2.gdx par=a rng=A2:B3613 rdim=1 par=r_direct rng=C2:D3613 rdim=1 par=d rng=I2:J3 rdim=1 par=area rng=K2:L3613 rdim=1 par=aratio rng=M2:N3613 rdim=1
$GDXIN D:/dx/BECCSBC/S2/abcparameters_simple2.gdx
$load a r_direct d area aratio
$GDXIN
;


Table d3(i,m)  distance between biomassi and bebcs plant m -km;
$call gdxxrw D:/dx/BECCSBC/S2/matrix_biomass_plant.xlsx output=D:/dx/BECCSBC/S2/matrix_biomass_plant.gdx par=d3  rdim=1 cdim=1
$GDXIN D:/dx/BECCSBC/S2/matrix_biomass_plant.gdx
$load d3
$GDXIN
;

*------------------unit costs of BE
Scalar f1                  transport cost of biomass per km t                   /1.3/ ;
Scalar C_harvest           Cost of harvesting the raw material t                /300/ ;



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


Parameter c3(i,m)  transport cost of biomass per t for bcs;
            c3(i,m) = f1 * d3(i,m)  ;
Parameter
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
          Cos22(n)
          Cos23(n)
          Cos26(n)
          ;

Variables
       wna(i,m)
       wa(i,m)                shipment total feedstocks from i to m
       bcsp(m)                quantities of bcs   plants in m
       zc1                    total costs of bcs use type1
       zc2

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

       realbcs(m)
       realbcs1(m)
       realbcs2(m)
       c_grid_cost3(i)
       c_grid_cost4(m)
       e_grid_emi2(i)
       c_grid_cost(i)
       c_grid_bcs(i,m)
       

       d_grid_bio2(i)                     
       d_Tbiotrans2
       
*       carbonlimit
       ;
Positive Variable wa,wna;

Integer Variable bcsp;

Equations

       T1CBCS                 Type1
       T2CBCS

     
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
       

       realm(m)
       realm1(m)
       realm2(m) 
       gridcost3(i) 
       gridcost4(m) 
       gridemi2(i)
       supply1(i)              observe supply limit at plantation i
       supply2(i) 
       soc(i)                 to maintain current SOC level
       arrival2(m)
       carbon                 carbon limit
       ;

*---------------cost considered only       
       T1CBCS..        zc1  =e=  z_bcs_har + z_bcs_biotrans + z_bcs_biochartrans + z_bcs_inv + z_bcs_om + z_bcs_appli;
*---------------cost - benefit (coal avoided only for beccs, biochar not considered for bcs)
       T2CBCS..        zc2  =e=  zc1 - b_bcs_oil - b_bcs_gas - b_bcs_yield;    

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
       realm1(m)..      realbcs1(m)=e=sum(i,wna(i,m));
       realm2(m)..      realbcs2(m)=e=sum(i,wa(i,m));
       realm(m)..       realbcs(m)=e=realbcs1(m)+realbcs2(m);

       gridcost3(i)..   c_grid_cost3(i)=e=sum(m, C_harvest*(wna(i,m)+wa(i,m))+ c3(i,m)*(wna(i,m)+wa(i,m))+ (c3(i,m)+C_bcs_appli)*TR_biochar*(wna(i,m)+wa(i,m)) + C_bcs_other*(wna(i,m)+wa(i,m))- TR_oil*(wna(i,m)+wa(i,m))*P_oil- TR_gas*(wna(i,m)+wa(i,m))*P_gas- (wna(i,m)+wa(i,m))*B_yield);
       gridcost4(m)..   c_grid_cost4(m)=e=C_bcs_inv*bcsp(m)+C_bcs_lm*bcsp(m);
       
       gridemi2(i)..    e_grid_emi2(i)=e=sum(m, (E_bcs_cap - E_bcs_lca-E_bcs_N2O)*(wna(i,m)+wa(i,m)));
*----------------constrains
       supply1(i) ..   sum(m, wa(i,m))  =l=  a(i)*aratio(i) ;
       supply2(i) ..   sum(m, wna(i,m))  =l=  a(i)*(1-aratio(i)) ;
       soc(i)$(area(i)and r_direct(i))..  sum(m, wa(i,m)+wna(i,m))*TR_biochar/r_biochar + (a(i)*aratio(i)-sum(m, wa(i,m)))/r_direct(i) =g= area1(i);
                     
       arrival2(m) ..  realbcs(m)  =l=  bcsp(m)* biopoten2 ;
       carbon..        e_bcs =g= carbonlimit;



Model transport /all/ ;
Option MIP = CPLEX;
Option resLim=10800;
Option optcr=0.05;
Option SolveLink=0;
$onecho > cplex.opt
threads 8
freegamsmodel  0
cuts -1
$offecho
transport.optfile=1;





LOOP(n,
carbonlimit=d(n);
Solve transport using MIP minimizing zc2 ;
*Solve transport using MIP maximizing carbonlimit ;
c_grid_bcs.l(i,m)=(c_grid_cost4.l(m)*(wna.l(i,m)+wa.l(i,m))/realbcs.l(m))$(realbcs.l(m)>0);
c_grid_cost.l(i)=((c_grid_cost3.l(i)+sum(m,c_grid_bcs.l(i,m)))/(e_grid_emi2.l(i)))$(e_grid_emi2.l(i)>0);

*----------------distances

d_Tbiotrans2.l = (sum((i,m), d3(i,m)*(wna.l(i,m)+wa.l(i,m)))/ sum((i,m), (wna.l(i,m)+wa.l(i,m))))$(sum((i,m),(wna.l(i,m)+wa.l(i,m)))>0);
d_grid_bio2.l(i)=(sum(m, d3(i,m)*(wna.l(i,m)+wa.l(i,m)))/sum(m,(wna.l(i,m)+wa.l(i,m))))$(sum(m,(wna.l(i,m)+wa.l(i,m)))>0);
         
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
Cos22(n)=e_bcs1.l   ;
Cos23(n)=e_bcs2.l   ;
Cos26(n)=d_Tbiotrans2.l;

    if(ord(n) = 1,
       Execute_Unload 'result_xy1.gdx', wa,bcsp,c_grid_cost,e_bcs,e_grid_emi2,d_grid_bio2,wna,area1;
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = wna Rng =Sheet21!';
       Execute 'Gdxxrw  result_xy1.gdx O = result_xy1.xlsx var = area1 Rng =Sheet23!';
       Execute_Unload 'result_z1.gdx', Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos22,Cos23,Cos26;
    );

    if(ord(n) = 2,
       Execute_Unload 'result_xy2.gdx', wa,bcsp,c_grid_cost,e_bcs,e_grid_emi2,d_grid_bio2,wna,area1;
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = wa Rng =Sheet3!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = bcsp Rng =Sheet5!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = c_grid_cost Rng =Sheet6!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_bcs Rng =Sheet7!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = e_grid_emi2 Rng =Sheet12!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = d_grid_bio2 Rng =Sheet19!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = wna Rng =Sheet21!';
       Execute 'Gdxxrw  result_xy2.gdx O = result_xy2.xlsx var = area1 Rng =Sheet23!';
       Execute_Unload 'result_z2.gdx', Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos22,Cos23,Cos26;
    );
);
Execute_Unload 'result_z.gdx', Cos10,Cos11,Cos12,Cos13,Cos14,Cos15,Cos16,Cos17,Cos18,Cos19,Cos22,Cos23,Cos26;



