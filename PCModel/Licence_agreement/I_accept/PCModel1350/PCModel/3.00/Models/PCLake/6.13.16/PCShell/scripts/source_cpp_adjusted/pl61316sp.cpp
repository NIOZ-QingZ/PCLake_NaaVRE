InitCalc = 0;
ConstDepth = 1;
InclTran = 1;
InclPhytS = 1;
InclBed = 1;
InclWeb = 1;
InclMarsh = 1;
InclSeason = 1;
ReadTemp = 0;
ReadLOut = 0;
ReadVWind = 0;
ReadQIn = 0;
ReadQOut = 0;
ReadQEv = 0;
ReadPLoad = 0;
ReadNLoad = 0;
ReadNutFrac = 0;
ReadPLoadPhyt = 0;
ReadDLoadDet = 0;
ReadDLoadIM = 0;
UseSeasonLoad = 0;
UsePulseLoad = 0;
mTemp = 0;
mLOut = 0;
mVWind = 0;
mQIn = 0;
mQOut = 0;
mQEv = 0;
mPLoad = 0;
mPLoadPO4 = 0;
mPLoadOrg = 0;
mPLoadPhytTot = 0;
mNLoad = 0;
mNLoadNH4 = 0;
mNLoadNO3 = 0;
mNLoadOrg = 0;
mDLoadDet = 0;
mDLoadIM = 0;
BeginTime = 0;
EndTime = 365;
YearZero = 0;
cFetch = 1000;
fMarsh = 0;
fLutum = 0.1;
fFeDIM = 0.01;
fAlDIM = 0.01;
cTmAve = 12;
cTmVar = 10;
cTimeLag = 40;
cVWind = 5;
cQInf = 0;
cPBackLoad = 0;
cNBackLoad = 0;
cLDayAve = 10000000;
cLDayVar = 8000000;
cfDayAve = 0.5;
cfDayVar = 0.2;
fRefl = 0.2;
cExtWat = 0.5;
cDredInterval = 9999000;
cDredStart = 9999000;
cDepthRef = 1E-28;
cLengDred = 10;
fEffDred = 0.95;
fEffDredBent = 0.5;
fPAR = 0.48;
cExtSpDet = 0.15;
cExtSpIM = 0.05;
fDTotS0 = 0.3;
fDOrgS0 = 0.1;
fDDetS0 = 0.05;
fSedPhyt0 = 0.01;
fPInorgS0 = 0.0005;
fPAdsS0 = 0.99;
cPDDet0 = 0.0025;
cNDDet0 = 0.025;
cSiDDet0 = 0.01;
cPDHum0 = 0.005;
cNDHum0 = 0.05;
cPDPhyt0 = 0.01;
cNDPhyt0 = 0.1;
cPDDiat0 = 0.01;
cNDDiat0 = 0.1;
cPDGren0 = 0.01;
cNDGren0 = 0.1;
cPDBlue0 = 0.01;
cNDBlue0 = 0.1;
cPDVeg0 = 0.002;
cNDVeg0 = 0.02;
cSiDDiat = 0.15;
cPDZooRef = 0.01;
cNDZooRef = 0.07;
cPDBentRef = 0.01;
cNDBentRef = 0.07;
cPDFishRef = 0.022;
cNDFishRef = 0.1;
cPDPisc = 0.022;
cNDPisc = 0.1;
cQIn = 20;
cQInSum = 20;
cQInWin = 20;
cDepthWMax = 5;
cQInExtraApril1 = 0;
cQInExtraOct1 = 0;
cQOutExtraApril1 = 0;
cQOutExtraOct1 = 0;
cQEvAve = 1.5;
cQEvVar = 1.3;
cPLoad = 0.005;
cPLoadSum = 0.005;
cPLoadWin = 0.005;
fPO4In = 0.5;
fPhytInWin = 0.02;
fPhytInSum = 0.1;
fDiatPhytIn = 0.33;
fGrenPhytIn = 0.34;
fBluePhytIn = 0.33;
cNLoad = 0.05;
cNLoadSum = 0.05;
cNLoadWin = 0.05;
cNPLoadMeas = 7;
cNPPhytIn = 7;
cNPDetIn = 7;
fNH4DissIn = 0.5;
cNDPhytIn = 0.07;
cNDDetIn = 0.07;
cDIMIn = 5;
cO2In = 5;
cSiO2In = 3;
cSiDDetIn = 0.05;
cDZooIn = 0.1;
cDayApril1 = 91;
cDayOct1 = 273;
cLengChange = 10;
cNLoadS = 0;
fNH4LoadS = 0.5;
cDErosTot = 0.1;
fSedErosIM = 0.95;
fDOrgSoil = 0.1;
cPDSoilOM = 0.001;
cNDSoilOM = 0.01;
cPO4Ground = 0.1;
cNH4Ground = 1;
cNO3Ground = 0.1;
cDepthS = 0.1;
cCPerDW = 0.4;
cRhoIM = 2500000;
cRhoOM = 1400000;
cTmRef = 20;
cAerRoot = 0.727;
cAerLin = -0.371;
cAerSquare = 0.0376;
cThetaAer = 1.024;
cVSetIM = 1;
cVSetDet = 0.25;
cThetaSet = 1.01;
cSuspMin = 6.1;
cSuspMax = 25.2;
cSuspSlope = 2.1;
hDepthSusp = 2;
cFetchRef = 1000;
fLutumRef = 0.2;
cSuspRef = 0.5;
kVegResus = 0.01;
kTurbFish = 1;
kResusPhytMax = 0.25;
cResusPhytExp = -0.379;
cThetaMinW = 1.07;
kDMinDetW = 0.01;
hO2BOD = 1;
O2PerNO3 = 1.5;
cThetaMinS = 1.07;
kDMinDetS = 0.002;
fRefrDetS = 0.15;
hNO3Denit = 2;
NO3PerC = 0.8;
kDMinHum = 0.00001;
kNitrW = 0.1;
kNitrS = 1;
cThetaNitr = 1.08;
O2PerNH4 = 2;
hO2Nitr = 2;
kPDifPO4 = 0.000072;
kNDifNO3 = 0.000086;
kNDifNH4 = 0.000112;
kO2Dif = 0.000026;
cThetaDif = 1.02;
fDepthDifS = 0.5;
cTurbDifNut = 5;
cTurbDifO2 = 5;
kPSorp = 0.05;
cRelPAdsD = 0.00003;
cRelPAdsFe = 0.065;
cRelPAdsAl = 0.134;
cKPAdsOx = 0.6;
fRedMax = 0.9;
coPO4Max = 1;
kPChemPO4 = 0.03;
cDayManVeg1 = -9999000;
cDayManVeg2 = -9999000;
fManVeg = 0;
cLengMan = 10;
cYearStartBirds = 0;
cDayStartBirds = 46;
cDayEndBirds = 288;
cBirdsPerha = 0;
cDGrazPerBird = 45;
hDVegBird = 5;
fDAssBird = 0.5;
fDissEgesBird = 0.25;
fDissMortVeg = 0.25;
cLengAllo = 15;
cLengMort = 15;
UseEmpUpt = 0;
fSedUptVegMax = 0.998;
fSedUptVegCoef = 2.66;
fSedUptVegExp = -0.83;
fRootVegSum = 0.1;
fRootVegWin = 0.6;
fFloatVeg = 0;
fEmergVeg = 0;
fDepth1Veg = 0;
fDepth2Veg = 1;
cDLayerVeg = 0;
cCovSpVeg = 0.5;
kMigrVeg = 0.001;
cDVegIn = 1;
cTmInitVeg = 9;
cDCarrVeg = 400;
cMuMaxVeg = 0.2;
cQ10ProdVeg = 1.2;
hLRefVeg = 17;
cExtSpVeg = 0.01;
kDRespVeg = 0.02;
cQ10RespVeg = 2;
kMortVegSum = 0.005;
fWinVeg = 0.3;
cDayWinVeg = 259;
fDetWMortVeg = 0.1;
cPrefVegBird = 1;
cVPUptMaxVeg = 0.01;
cAffPUptVeg = 0.2;
cPDVegMin = 0.0008;
cPDVegMax = 0.0035;
cVNUptMaxVeg = 0.1;
cAffNUptVeg = 0.2;
cNDVegMin = 0.01;
cNDVegMax = 0.035;
cPACoefMin = 1.5;
cPACoefMax = 2.5;
hPACoef = 3;
cSecchiPlus = 0;
cEuph = 1.7;
cCovSpPhyt = 2;
cTmOptLoss = 25;
cSigTmLoss = 13;
fDissMortPhyt = 0.2;
fDissLoss = 0.25;
cMuMaxDiat = 2;
cTmOptDiat = 18;
cSigTmDiat = 20;
cExtSpDiat = 0.25;
UseSteeleDiat = 1;
cLOptRefDiat = 54;
hLRefDiat = 1000;
cChDDiatMin = 0.004;
cChDDiatMax = 0.012;
kDRespDiat = 0.1;
kLossDiat = 0.25;
kMortDiatW = 0.01;
kMortDiatS = 0.05;
cVSetDiat = 0.5;
cVPUptMaxDiat = 0.01;
cAffPUptDiat = 0.2;
cPDDiatMin = 0.0005;
cPDDiatMax = 0.005;
cVNUptMaxDiat = 0.07;
cAffNUptDiat = 0.2;
cNDDiatMin = 0.01;
cNDDiatMax = 0.05;
hSiAssDiat = 0.09;
cMuMaxGren = 1.5;
cTmOptGren = 25;
cSigTmGren = 15;
cExtSpGren = 0.25;
UseSteeleGren = 0;
hLRefGren = 17;
cLOptRefGren = 1000;
cChDGrenMin = 0.01;
cChDGrenMax = 0.02;
kDRespGren = 0.075;
kLossGren = 0.25;
kMortGrenW = 0.01;
kMortGrenS = 0.05;
cVSetGren = 0.2;
cVPUptMaxGren = 0.01;
cAffPUptGren = 0.2;
cPDGrenMin = 0.0015;
cPDGrenMax = 0.015;
cVNUptMaxGren = 0.07;
cAffNUptGren = 0.2;
cNDGrenMin = 0.02;
cNDGrenMax = 0.1;
hSiAssGren = 0;
cMuMaxBlue = 0.6;
cTmOptBlue = 25;
cSigTmBlue = 12;
cExtSpBlue = 0.35;
UseSteeleBlue = 1;
cLOptRefBlue = 13.6;
hLRefBlue = 1000;
cChDBlueMin = 0.005;
cChDBlueMax = 0.015;
cCyDBlueMin = 0.004;
cCyDBlueMax = 0.06;
kDRespBlue = 0.03;
kLossBlue = 0.03;
kMortBlueW = 0.01;
kMortBlueS = 0.2;
cVSetBlue = 0.06;
cVPUptMaxBlue = 0.04;
cAffPUptBlue = 0.8;
cPDBlueMin = 0.0025;
cPDBlueMax = 0.025;
cVNUptMaxBlue = 0.07;
cAffNUptBlue = 0.2;
cNDBlueMin = 0.03;
cNDBlueMax = 0.15;
hSiAssBlue = 0;
cDBentIn = 0.01;
kMigrBent = 0.001;
kMigrFish = 0.001;
cDFiJvIn = 0.005;
cDFiAdIn = 0.005;
kHarvFishWin = 0;
kHarvFishSum = 0;
cDPiscIn = 0.001;
kMigrPisc = 0.001;
kHarvPiscWin = 0;
kHarvPiscSum = 0;
cFiltMax = 4.5;
hFilt = 1;
cDCarrZoo = 25;
cPrefDiat = 0.75;
cPrefGren = 0.75;
cPrefBlue = 0.125;
cPrefDet = 0.25;
fDAssZoo = 0.35;
fDissEgesZoo = 0.25;
kDRespZoo = 0.15;
kMortZoo = 0.04;
fDissMortZoo = 0.1;
cTmOptZoo = 25;
cSigTmZoo = 13;
cDCarrBent = 10;
kDAssBent = 0.1;
hDFoodBent = 200;
fDAssBent = 0.3;
fDissEgesBent = 0.25;
kDRespBent = 0.005;
kMortBent = 0.005;
fDissMortBent = 0.1;
cTmOptBent = 25;
cSigTmBent = 16;
fDBone = 0.35;
fPBone = 0.5;
cDCarrFish = 15;
fDissEgesFish = 0.25;
fDissMortFish = 0.1;
cTmOptFish = 25;
cSigTmFish = 10;
cDayReprFish = 120;
fReprFish = 0.02;
fAgeFish = 0.5;
cRelVegFish = 0.009;
kDAssFiJv = 0.12;
hDZooFiJv = 1.25;
fDAssFiJv = 0.4;
kDRespFiJv = 0.01;
kMortFiJv = 0.00137;
kDAssFiAd = 0.06;
hDBentFiAd = 2.5;
fDAssFiAd = 0.4;
kDRespFiAd = 0.004;
kMortFiAd = 0.00027;
cDCarrPiscMax = 1.2;
cDCarrPiscMin = 0.1;
cDCarrPiscBare = 0.1;
cDPhraMinPisc = 50;
cCovVegMin = 40;
cRelPhraPisc = 0.075;
cRelVegPisc = 0.03;
kDAssPisc = 0.025;
hDVegPisc = 5;
hDFishPisc = 1;
fDAssPisc = 0.4;
fDissEgesPisc = 0.25;
kDRespPisc = 0.005;
kMortPisc = 0.00027;
fDissMortPisc = 0.1;
cTmOptPisc = 25;
cSigTmPisc = 10;
cDepthSM = 0.1;
kExchMaxM = 1;
hfMarsh = 0.1;
fDTotSM0 = 0.3;
fDOrgSM0 = 0.1;
fDDetSM0 = 0.05;
fPInorgSM0 = 0.0005;
cPDPhra0 = 0.002;
cNDPhra0 = 0.02;
cDensStemPhra = 61.5;
cTmInitPhra = 8;
fDAllPhra = 0.3;
kDAllPhra = 0.05;
cDStemPhra = 6;
cQ10ProdPhra = 2;
cMuPhraMax = 0.03;
cDShootPhraMax = 3500;
cCovSpPhra = 0.1;
cPDPhraMin = 0.0008;
cPDPhraMax = 0.003;
cNDPhraMin = 0.008;
cNDPhraMax = 0.03;
cAffNUptPhra = 0.0002;
cAffPUptPhra = 0.0002;
cVNUptPhraMax = 0.1;
cVPUptPhraMax = 0.01;
kDRespPhra = 0.001;
cQ10RespPhra = 2.5;
fDayWin = 0.52;
fDRealPhra = 0.85;
kDRealPhra = 0.05;
kDMortShootPhra = 0;
kDMortRootPhra = 0.000391;
cDayWinPhra = 259;
cDayManPhra = 255;
fManPhra = 0;
kDManShootPhra = 1;
DaysPerYear = 365;
TenDays = 10;
HoursPerDay = 24;
SecsPerDay = 86400;
mmPerm = 1000;
m2Perha = 10000;
mgPerg = 1000;
gPerkg = 1000;
gPerton = 1000000;
PerCent = 0.01;
NearZero = 1E-28;
molO2molC = 2.6667;
molO2molN = 2.2857;
molNmolC = 1.1667;
cRhoWat = 1000000;
Pi = 3.14159265358979;
