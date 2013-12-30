/********************************************************/
/********************************************************/
/*	Source file: Veg_Prod.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 vegetation production parameter information.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 11/16/2010	(drs) added LAIforest, biofoliage_for, lai_conv_for, TypeGrassOrShrub, TypeForest to SW_VEGPROD
 lai_live/biolive/total_agb include now LAIforest, respectively biofoliage_for
 updated SW_VPD_read(), SW_VPD_init(), and _echo_inits()
 increased length of char outstr[1000] to outstr[1500] because of increased echo
 02/22/2011	(drs) added scan for litter_for to SW_VPD_read()
 02/22/2011	(drs) added litter_for to SW_VegProd.litter and to SW_VegProd.tot_agb
 02/22/2011	(drs) if TypeGrassOrShrub is turned off, then its biomass, litter, etc. values are set to 0
 08/22/2011	(drs) use variable veg_height [MAX_MONTHS] from SW_VEGPROD instead of static canopy_ht
 09/08/2011	(drs) adapted SW_VPD_read() and SW_VPD_init() to reflect that now each vegetation type has own elements
 09/08/2011	(drs) added input in SW_VPD_read() of tanfunc_t tr_shade_effects, and RealD shade_scale and shade_deadmax (they were previously hidden as constants in code in SW_Flow_lib.h)
 09/08/2011	(drs) moved all input of hydraulic redistribution variables from SW_Site.c to SW_VPD_read() for each vegetation type
 09/08/2011	(drs) added input in SW_VPD_read() of RealD veg_intPPT_a, veg_intPPT_b, veg_intPPT_c, veg_intPPT_d (they were previously hidden as constants in code in SW_Flow_lib.h)
 09/09/2011	(drs) added input in SW_VPD_read() of RealD EsTpartitioning_param (it were previously hidden as constant in code in SW_Flow_lib.h)
 09/09/2011	(drs) added input in SW_VPD_read() of RealD Es_param_limit (it was previously hidden as constant in code in SW_Flow_lib.h)
 09/13/2011	(drs) added input in SW_VPD_read() of RealD litt_intPPT_a, litt_intPPT_b, litt_intPPT_c, litt_intPPT_d (they were previously hidden as constants in code in SW_Flow_lib.h)
 09/13/2011	(drs) added input in SW_VPD_read() of RealD canopy_height_constant and updated SW_VPD_init() (as option: if > 0 then constant canopy height (cm) and overriding cnpy-tangens=f(biomass))
 09/15/2011	(drs) added input in SW_VPD_read() of RealD albedo
 09/26/2011	(drs)	added calls to Times.c:interpolate_monthlyValues() in SW_VPD_init() for each monthly input variable; replaced monthly loop with a daily loop for additional daily variables; adjusted _echo_inits()
 10/17/2011	(drs)	in SW_VPD_init(): v->tree.total_agb_daily[doy] = v->tree.litter_daily[doy] + v->tree.biolive_daily[doy] instead of = v->tree.litter_daily[doy] + v->tree.biomass_daily[doy] to adjust for better scaling of potential bare-soil evaporation
 02/04/2012	(drs) 	added input in SW_VPD_read() of RealD SWPcrit
 01/29/2013	(clk)	changed the read in to account for the extra fractional component in total vegetation, bare ground
 added the variable RealF help_bareGround as a place holder for the sscanf call.
 01/31/2013	(clk)	changed the read in to account for the albedo for bare ground, storing the input in bareGround_albedo
 changed _echo_inits() to now display the bare ground components in logfile.log
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_VPD_read()
 07/09/2013	(clk)	added initialization of all the values of the new vegtype variable forb and fractionForb
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "generic.h"
#include "filefuncs.h"
#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Times.h"
#include "SW_VegProd.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern Bool EchoInits;
#ifdef RSOILWAT
extern Bool collectInData;
#endif
SW_VEGPROD SW_VegProd; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

static RealD lai_standing; /*standing crop lai (g/m**2)     */

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static void _echo_inits(void);

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_VPD_read(void) {
	/* =================================================== */
	SW_VEGPROD *v = &SW_VegProd;
	FILE *f;
	Months mon = Jan;
	int x, lineno = 0;
	const int line_help = 29;
	RealF help_grass, help_shrub, help_tree, help_forb, help_bareGround, litt, biom, pctl, laic;
	RealD fraction_sum = 0.;

	MyFileName = SW_F_name(eVegProd);
	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {
		if (lineno++ < line_help) {
			switch (lineno) {
			/* fractions of vegetation types */
			case 1:
				x = sscanf(inbuf, "%f %f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb, &help_bareGround);
				if (x < 5) {
					sprintf(errstr, "ERROR: invalid record in vegetation type components in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->fractionGrass = help_grass;
				v->fractionShrub = help_shrub;
				v->fractionTree = help_tree;
				v->fractionForb = help_forb;
				v->fractionBareGround = help_bareGround;
				break;

				/* albedo */
			case 2:
				x = sscanf(inbuf, "%f %f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb, &help_bareGround);
				if (x < 5) {
					sprintf(errstr, "ERROR: invalid record in albedo values in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.albedo = help_grass;
				v->shrub.albedo = help_shrub;
				v->tree.albedo = help_tree;
				v->forb.albedo = help_forb;
				v->bareGround_albedo = help_bareGround;
				break;

				/* LAI converter for % cover */
			case 3:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in percent cover converting in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.conv_stcr = help_grass;
				v->shrub.conv_stcr = help_shrub;
				v->tree.conv_stcr = help_tree;
				v->forb.conv_stcr = help_forb;
				break;

				/* canopy height */
			case 4:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in canopy xinflec in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.cnpy.xinflec = help_grass;
				v->shrub.cnpy.xinflec = help_shrub;
				v->tree.cnpy.xinflec = help_tree;
				v->forb.cnpy.xinflec = help_forb;
				break;
			case 5:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in canopy yinflec in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.cnpy.yinflec = help_grass;
				v->shrub.cnpy.yinflec = help_shrub;
				v->tree.cnpy.yinflec = help_tree;
				v->forb.cnpy.yinflec = help_forb;
				break;
			case 6:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in canopy range in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.cnpy.range = help_grass;
				v->shrub.cnpy.range = help_shrub;
				v->tree.cnpy.range = help_tree;
				v->forb.cnpy.range = help_forb;
				break;
			case 7:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in canopy slope in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.cnpy.slope = help_grass;
				v->shrub.cnpy.slope = help_shrub;
				v->tree.cnpy.slope = help_tree;
				v->forb.cnpy.slope = help_forb;
				break;
			case 8:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in canopy height constant option in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.canopy_height_constant = help_grass;
				v->shrub.canopy_height_constant = help_shrub;
				v->tree.canopy_height_constant = help_tree;
				v->forb.canopy_height_constant = help_forb;
				break;

				/* vegetation interception parameters */
			case 9:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in interception parameter a in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.veg_intPPT_a = help_grass;
				v->shrub.veg_intPPT_a = help_shrub;
				v->tree.veg_intPPT_a = help_tree;
				v->forb.veg_intPPT_a = help_forb;
				break;
			case 10:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in interception parameter b in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.veg_intPPT_b = help_grass;
				v->shrub.veg_intPPT_b = help_shrub;
				v->tree.veg_intPPT_b = help_tree;
				v->forb.veg_intPPT_b = help_forb;
				break;
			case 11:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in interception parameter c in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.veg_intPPT_c = help_grass;
				v->shrub.veg_intPPT_c = help_shrub;
				v->tree.veg_intPPT_c = help_tree;
				v->forb.veg_intPPT_c = help_forb;
				break;
			case 12:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in interception parameter d in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.veg_intPPT_d = help_grass;
				v->shrub.veg_intPPT_d = help_shrub;
				v->tree.veg_intPPT_d = help_tree;
				v->forb.veg_intPPT_d = help_forb;
				break;

				/* litter interception parameters */
			case 13:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in litter interception parameter a in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.litt_intPPT_a = help_grass;
				v->shrub.litt_intPPT_a = help_shrub;
				v->tree.litt_intPPT_a = help_tree;
				v->forb.litt_intPPT_a = help_forb;
				break;
			case 14:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in litter interception parameter b in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.litt_intPPT_b = help_grass;
				v->shrub.litt_intPPT_b = help_shrub;
				v->tree.litt_intPPT_b = help_tree;
				v->forb.litt_intPPT_b = help_forb;
				break;
			case 15:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in litter interception parameter c in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.litt_intPPT_c = help_grass;
				v->shrub.litt_intPPT_c = help_shrub;
				v->tree.litt_intPPT_c = help_tree;
				v->forb.litt_intPPT_c = help_forb;
				break;
			case 16:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in litter interception parameter d in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.litt_intPPT_d = help_grass;
				v->shrub.litt_intPPT_d = help_shrub;
				v->tree.litt_intPPT_d = help_tree;
				v->forb.litt_intPPT_d = help_forb;
				break;

				/* parameter for partitioning of bare-soil evaporation and transpiration */
			case 17:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in parameter for partitioning of bare-soil evaporation and transpiration in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.EsTpartitioning_param = help_grass;
				v->shrub.EsTpartitioning_param = help_shrub;
				v->tree.EsTpartitioning_param = help_tree;
				v->forb.EsTpartitioning_param = help_forb;
				break;

				/* Parameter for scaling and limiting bare soil evaporation rate */
			case 18:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in parameter for Parameter for scaling and limiting bare soil evaporation rate in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.Es_param_limit = help_grass;
				v->shrub.Es_param_limit = help_shrub;
				v->tree.Es_param_limit = help_tree;
				v->forb.Es_param_limit = help_forb;
				break;

				/* shade effects */
			case 19:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade scale in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.shade_scale = help_grass;
				v->shrub.shade_scale = help_shrub;
				v->tree.shade_scale = help_tree;
				v->forb.shade_scale = help_forb;
				break;
			case 20:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade max dead biomass in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.shade_deadmax = help_grass;
				v->shrub.shade_deadmax = help_shrub;
				v->tree.shade_deadmax = help_tree;
				v->forb.shade_deadmax = help_forb;
				break;
			case 21:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade xinflec in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.tr_shade_effects.xinflec = help_grass;
				v->shrub.tr_shade_effects.xinflec = help_shrub;
				v->tree.tr_shade_effects.xinflec = help_tree;
				v->forb.tr_shade_effects.xinflec = help_forb;
				break;
			case 22:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade yinflec in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.tr_shade_effects.yinflec = help_grass;
				v->shrub.tr_shade_effects.yinflec = help_shrub;
				v->tree.tr_shade_effects.yinflec = help_tree;
				v->forb.tr_shade_effects.yinflec = help_forb;
				break;
			case 23:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade range in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.tr_shade_effects.range = help_grass;
				v->shrub.tr_shade_effects.range = help_shrub;
				v->tree.tr_shade_effects.range = help_tree;
				v->forb.tr_shade_effects.range = help_forb;
				break;
			case 24:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in shade slope in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.tr_shade_effects.slope = help_grass;
				v->shrub.tr_shade_effects.slope = help_shrub;
				v->tree.tr_shade_effects.slope = help_tree;
				v->forb.tr_shade_effects.slope = help_forb;
				break;

				/* Hydraulic redistribution */
			case 25:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in hydraulic redistribution: flag in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.flagHydraulicRedistribution = help_grass;
				v->shrub.flagHydraulicRedistribution = help_shrub;
				v->tree.flagHydraulicRedistribution = help_tree;
				v->forb.flagHydraulicRedistribution = help_forb;
				break;
			case 26:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in hydraulic redistribution: maxCondroot in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.maxCondroot = help_grass;
				v->shrub.maxCondroot = help_shrub;
				v->tree.maxCondroot = help_tree;
				v->forb.maxCondroot = help_forb;
				break;
			case 27:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in hydraulic redistribution: swpMatric50 in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.swpMatric50 = help_grass;
				v->shrub.swpMatric50 = help_shrub;
				v->tree.swpMatric50 = help_tree;
				v->forb.swpMatric50 = help_forb;
				break;
			case 28:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in hydraulic redistribution: shapeCond in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.shapeCond = help_grass;
				v->shrub.shapeCond = help_shrub;
				v->tree.shapeCond = help_tree;
				v->forb.shapeCond = help_forb;
				break;

				/* Critical soil water potential */
			case 29:
				x = sscanf(inbuf, "%f %f %f %f", &help_grass, &help_shrub, &help_tree, &help_forb);
				if (x < 4) {
					sprintf(errstr, "ERROR: invalid record in critical soil water potentials: flag in %s\n", MyFileName);
					CloseFile(&f);
					LogError(logfp, LOGFATAL, errstr);
				}
				v->grass.SWPcrit = -10. * help_grass;
				v->shrub.SWPcrit = -10. * help_shrub;
				v->tree.SWPcrit = -10. * help_tree;
				v->forb.SWPcrit = -10. * help_forb;
				break;

			default:
				break;
			}
		} else {
			if (lineno == line_help + 1 || lineno == line_help + 1 + 12 || lineno == line_help + 1 + 12 * 2 || lineno == line_help + 1 + 12 * 3)
				mon = Jan;

			x = sscanf(inbuf, "%f %f %f %f", &litt, &biom, &pctl, &laic);
			if (x < 4) {
				sprintf(errstr, "ERROR: invalid record %d in %s\n", mon + 1, MyFileName);
				CloseFile(&f);
				LogError(logfp, LOGFATAL, errstr);
			}
			if (lineno > line_help + 12 * 3 && lineno <= line_help + 12 * 4) {
				v->forb.litter[mon] = litt;
				v->forb.biomass[mon] = biom;
				v->forb.pct_live[mon] = pctl;
				v->forb.lai_conv[mon] = laic;
			} else if (lineno > line_help + 12 * 2 && lineno <= line_help + 12 * 3) {
				v->tree.litter[mon] = litt;
				v->tree.biomass[mon] = biom;
				v->tree.pct_live[mon] = pctl;
				v->tree.lai_conv[mon] = laic;
			} else if (lineno > line_help + 12 && lineno <= line_help + 12 * 2) {
				v->shrub.litter[mon] = litt;
				v->shrub.biomass[mon] = biom;
				v->shrub.pct_live[mon] = pctl;
				v->shrub.lai_conv[mon] = laic;
			} else if (lineno > line_help && lineno <= line_help + 12) {
				v->grass.litter[mon] = litt;
				v->grass.biomass[mon] = biom;
				v->grass.pct_live[mon] = pctl;
				v->grass.lai_conv[mon] = laic;
			}

			mon++;

		}
	}

	if (mon < Dec) {
		sprintf(errstr, "No Veg Production values after month %d", mon + 1);
		LogError(logfp, LOGWARN, errstr);
	}

	fraction_sum = v->fractionGrass + v->fractionShrub + v->fractionTree + v->fractionForb + v->fractionBareGround;
	if (!EQ(fraction_sum, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Fractions of vegetation components were normalized, "
				"sum of fractions (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, fraction_sum);
		v->fractionGrass /= fraction_sum;
		v->fractionShrub /= fraction_sum;
		v->fractionTree /= fraction_sum;
		v->fractionBareGround /= fraction_sum;
		v->fractionForb /= fraction_sum;
		LogError(logfp, LOGWARN, "  Grassland fraction : %5.4f", v->fractionGrass);
		LogError(logfp, LOGWARN, "  Shrubland fraction : %5.4f", v->fractionShrub);
		LogError(logfp, LOGWARN, "  Forest/tree fraction : %5.4f", v->fractionTree);
		LogError(logfp, LOGWARN, "  FORB fraction : %5.4f", v->fractionForb);
		LogError(logfp, LOGWARN, "  Bare Ground fraction : %5.4f", v->fractionBareGround);
	}

	CloseFile(&f);
#ifdef RSOILWAT
	if (!collectInData)
#endif
		SW_VPD_init();

	if (EchoInits)
		_echo_inits();
}
#ifdef RSOILWAT
SEXP onGet_SW_VPD() {
	int i;
	SW_VEGPROD *v = &SW_VegProd;
	SEXP swProd;
	SEXP VegProd, VegProd_names;
	char *cVegProd_names[] = { "Composition", "Albedo", "Cover_stcr", "CanopyHeight", "VegetationInterceptionParameters", "LitterInterceptionParameters",
			"EsTpartitioning_param", "Es_param_limit", "Shade", "HydraulicRedistribution_use", "HydraulicRedistribution", "CriticalSoilWaterPotential",
			"MonthlyProductionValues_grass", "MonthlyProductionValues_shrub", "MonthlyProductionValues_tree", "MonthlyProductionValues_forb" };

	SEXP VegComp, VegComp_names;
	SEXP Albedo;
	SEXP conv_stcr, col_names;

	SEXP Canopy, Canopy_names, Canopy_names_x;
	char *cCanopy_names_x[] = { "xinflec", "yinflec", "range", "slope", "height_cm" };
	RealD *p_Canopy;

	SEXP VegInterception, VegInterception_names, VegInterception_names_x;
	char *cVegInterception_x[] = { "a", "b", "c", "d" };
	RealD *p_VegInterception;

	SEXP LitterInterception, LitterInterception_names, LitterInterception_names_x;
	char *cLitterInterception_x[] = { "a", "b", "c", "d" };
	RealD *p_LitterInterception;

	SEXP EsTpartitioning_param;
	SEXP Es_param_limit;

	SEXP Shade, Shade_names, Shade_names_x;
	char *cShade_names_x[] = { "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec", "yinflec", "range", "slope" };
	RealD *p_Shade;

	SEXP Hydraulic_flag;//"Flag"
	SEXP Hydraulic, Hydraulic_names, Hydraulic_names_x;
	RealD *p_Hydraulic;
	char *cHydraulic_names[] = { "MaxCondRoot", "SoilWaterPotential50", "ShapeCond" };

	SEXP CSWP;

	SEXP Grasslands, Grasslands_names;
	SEXP Shrublands, Shrublands_names;
	SEXP Forest, Forest_names;
	SEXP Forb, Forb_names;
	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;
	SEXP MonthlyProductionValues_Column_names, MonthlyProductionValues_Row_names;
	char *cMonthlyProductionValues_Column_names[] = { "Litter", "Biomass", "Live_pct", "LAI_conv" };
	char *cMonths[] = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };

	PROTECT(swProd = MAKE_CLASS("swProd"));
	PROTECT(VegProd = NEW_OBJECT(swProd));

	PROTECT(VegComp = allocVector(REALSXP, 5));
	REAL(VegComp)[0] = v->fractionGrass; //Grass
	REAL(VegComp)[1] = v->fractionShrub; //Shrub
	REAL(VegComp)[2] = v->fractionTree; //Tree
	REAL(VegComp)[3] = v->fractionForb; //forb
	REAL(VegComp)[4] = v->fractionBareGround; //Bare Ground
	PROTECT(VegComp_names = allocVector(STRSXP, 5));
	SET_STRING_ELT(VegComp_names, 0, mkChar("Grasses"));
	SET_STRING_ELT(VegComp_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(VegComp_names, 2, mkChar("Trees"));
	SET_STRING_ELT(VegComp_names, 3, mkChar("Forbs"));
	SET_STRING_ELT(VegComp_names, 4, mkChar("Bare Ground"));
	setAttrib(VegComp, R_NamesSymbol, VegComp_names);

	PROTECT(Albedo = allocVector(REALSXP, 5));
	REAL(Albedo)[0] = v->grass.albedo; //Grass
	REAL(Albedo)[1] = v->shrub.albedo; //Shrub
	REAL(Albedo)[2] = v->tree.albedo; //Tree
	REAL(Albedo)[3] = v->forb.albedo; //forb
	REAL(Albedo)[4] = v->bareGround_albedo; //bare ground
	setAttrib(Albedo, R_NamesSymbol, VegComp_names);

	PROTECT(conv_stcr = allocVector(REALSXP, 4));
	REAL(conv_stcr)[0] = v->grass.conv_stcr; //Grass
	REAL(conv_stcr)[1] = v->shrub.conv_stcr; //Shrub
	REAL(conv_stcr)[2] = v->tree.conv_stcr; //Tree
	REAL(conv_stcr)[3] = v->forb.conv_stcr; //forb
	PROTECT(col_names = allocVector(STRSXP, 4));
	SET_STRING_ELT(col_names, 0, mkChar("Grasses"));
	SET_STRING_ELT(col_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(col_names, 2, mkChar("Trees"));
	SET_STRING_ELT(col_names, 3, mkChar("Forbs"));
	setAttrib(conv_stcr, R_NamesSymbol, col_names);

	PROTECT(Canopy = allocMatrix(REALSXP, 5, 4));
	p_Canopy = REAL(Canopy);
	p_Canopy[0] = v->grass.cnpy.xinflec;
	p_Canopy[1] = v->grass.cnpy.yinflec;
	p_Canopy[2] = v->grass.cnpy.range;
	p_Canopy[3] = v->grass.cnpy.slope;
	p_Canopy[4] = v->grass.canopy_height_constant;
	p_Canopy[5] = v->shrub.cnpy.xinflec;
	p_Canopy[6] = v->shrub.cnpy.yinflec;
	p_Canopy[7] = v->shrub.cnpy.range;
	p_Canopy[8] = v->shrub.cnpy.slope;
	p_Canopy[9] = v->shrub.canopy_height_constant;
	p_Canopy[10] = v->tree.cnpy.xinflec;
	p_Canopy[11] = v->tree.cnpy.yinflec;
	p_Canopy[12] = v->tree.cnpy.range;
	p_Canopy[13] = v->tree.cnpy.slope;
	p_Canopy[14] = v->tree.canopy_height_constant;
	p_Canopy[15] = v->forb.cnpy.xinflec;
	p_Canopy[16] = v->forb.cnpy.yinflec;
	p_Canopy[17] = v->forb.cnpy.range;
	p_Canopy[18] = v->forb.cnpy.slope;
	p_Canopy[19] = v->forb.canopy_height_constant;
	PROTECT(Canopy_names = allocVector(VECSXP, 2));
	PROTECT(Canopy_names_x = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(Canopy_names_x, i, mkChar(cCanopy_names_x[i]));
	SET_VECTOR_ELT(Canopy_names, 0, Canopy_names_x);
	SET_VECTOR_ELT(Canopy_names, 1, col_names);
	setAttrib(Canopy, R_DimNamesSymbol, Canopy_names);

	PROTECT(VegInterception = allocMatrix(REALSXP, 4, 4));
	p_VegInterception = REAL(VegInterception);
	p_VegInterception[0] = v->grass.veg_intPPT_a;
	p_VegInterception[1] = v->grass.veg_intPPT_b;
	p_VegInterception[2] = v->grass.veg_intPPT_c;
	p_VegInterception[3] = v->grass.veg_intPPT_d;
	p_VegInterception[4] = v->shrub.veg_intPPT_a;
	p_VegInterception[5] = v->shrub.veg_intPPT_b;
	p_VegInterception[6] = v->shrub.veg_intPPT_c;
	p_VegInterception[7] = v->shrub.veg_intPPT_d;
	p_VegInterception[8] = v->tree.veg_intPPT_a;
	p_VegInterception[9] = v->tree.veg_intPPT_b;
	p_VegInterception[10] = v->tree.veg_intPPT_c;
	p_VegInterception[11] = v->tree.veg_intPPT_d;
	p_VegInterception[12] = v->forb.veg_intPPT_a;
	p_VegInterception[13] = v->forb.veg_intPPT_b;
	p_VegInterception[14] = v->forb.veg_intPPT_c;
	p_VegInterception[15] = v->forb.veg_intPPT_d;
	PROTECT(VegInterception_names = allocVector(VECSXP, 2));
	PROTECT(VegInterception_names_x = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(VegInterception_names_x, i, mkChar(cVegInterception_x[i]));
	SET_VECTOR_ELT(VegInterception_names, 0, VegInterception_names_x);
	SET_VECTOR_ELT(VegInterception_names, 1, col_names);
	setAttrib(VegInterception, R_DimNamesSymbol, VegInterception_names);

	PROTECT(LitterInterception = allocMatrix(REALSXP, 4, 4));
	p_LitterInterception = REAL(LitterInterception);
	p_LitterInterception[0] = v->grass.litt_intPPT_a;
	p_LitterInterception[1] = v->grass.litt_intPPT_b;
	p_LitterInterception[2] = v->grass.litt_intPPT_c;
	p_LitterInterception[3] = v->grass.litt_intPPT_d;
	p_LitterInterception[4] = v->shrub.litt_intPPT_a;
	p_LitterInterception[5] = v->shrub.litt_intPPT_b;
	p_LitterInterception[6] = v->shrub.litt_intPPT_c;
	p_LitterInterception[7] = v->shrub.litt_intPPT_d;
	p_LitterInterception[8] = v->tree.litt_intPPT_a;
	p_LitterInterception[9] = v->tree.litt_intPPT_b;
	p_LitterInterception[10] = v->tree.litt_intPPT_c;
	p_LitterInterception[11] = v->tree.litt_intPPT_d;
	p_LitterInterception[12] = v->forb.litt_intPPT_a;
	p_LitterInterception[13] = v->forb.litt_intPPT_b;
	p_LitterInterception[14] = v->forb.litt_intPPT_c;
	p_LitterInterception[15] = v->forb.litt_intPPT_d;
	PROTECT(LitterInterception_names = allocVector(VECSXP, 2));
	PROTECT(LitterInterception_names_x = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(LitterInterception_names_x, i, mkChar(cLitterInterception_x[i]));
	SET_VECTOR_ELT(LitterInterception_names, 0, LitterInterception_names_x);
	SET_VECTOR_ELT(LitterInterception_names, 1, col_names);
	setAttrib(LitterInterception, R_DimNamesSymbol, LitterInterception_names);

	PROTECT(EsTpartitioning_param = allocVector(REALSXP, 4));
	REAL(EsTpartitioning_param)[0] = v->grass.EsTpartitioning_param; //Grass
	REAL(EsTpartitioning_param)[1] = v->shrub.EsTpartitioning_param; //Shrub
	REAL(EsTpartitioning_param)[2] = v->tree.EsTpartitioning_param; //Tree
	REAL(EsTpartitioning_param)[3] = v->forb.EsTpartitioning_param; //forb
	setAttrib(EsTpartitioning_param, R_NamesSymbol, col_names);

	PROTECT(Es_param_limit = allocVector(REALSXP, 4));
	REAL(Es_param_limit)[0] = v->grass.Es_param_limit; //Grass
	REAL(Es_param_limit)[1] = v->shrub.Es_param_limit; //Shrub
	REAL(Es_param_limit)[2] = v->tree.Es_param_limit; //Tree
	REAL(Es_param_limit)[3] = v->forb.Es_param_limit; //forb
	setAttrib(Es_param_limit, R_NamesSymbol, col_names);

	PROTECT(Shade = allocMatrix(REALSXP, 6, 4));
	p_Shade = REAL(Shade);
	p_Shade[0] = v->grass.shade_scale;
	p_Shade[1] = v->grass.shade_deadmax;
	p_Shade[2] = v->grass.tr_shade_effects.xinflec;
	p_Shade[3] = v->grass.tr_shade_effects.yinflec;
	p_Shade[4] = v->grass.tr_shade_effects.range;
	p_Shade[5] = v->grass.tr_shade_effects.slope;
	p_Shade[6] = v->shrub.shade_scale;
	p_Shade[7] = v->shrub.shade_deadmax;
	p_Shade[8] = v->shrub.tr_shade_effects.xinflec;
	p_Shade[9] = v->shrub.tr_shade_effects.yinflec;
	p_Shade[10] = v->shrub.tr_shade_effects.range;
	p_Shade[11] = v->shrub.tr_shade_effects.slope;
	p_Shade[12] = v->tree.shade_scale;
	p_Shade[13] = v->tree.shade_deadmax;
	p_Shade[14] = v->tree.tr_shade_effects.xinflec;
	p_Shade[15] = v->tree.tr_shade_effects.yinflec;
	p_Shade[16] = v->tree.tr_shade_effects.range;
	p_Shade[17] = v->tree.tr_shade_effects.slope;
	p_Shade[18] = v->forb.shade_scale;
	p_Shade[19] = v->forb.shade_deadmax;
	p_Shade[20] = v->forb.tr_shade_effects.xinflec;
	p_Shade[21] = v->forb.tr_shade_effects.yinflec;
	p_Shade[22] = v->forb.tr_shade_effects.range;
	p_Shade[23] = v->forb.tr_shade_effects.slope;
	PROTECT(Shade_names = allocVector(VECSXP, 2));
	PROTECT(Shade_names_x = allocVector(STRSXP, 6));
	for (i = 0; i < 6; i++)
		SET_STRING_ELT(Shade_names_x, i, mkChar(cShade_names_x[i]));
	SET_VECTOR_ELT(Shade_names, 0, Shade_names_x);
	SET_VECTOR_ELT(Shade_names, 1, col_names);
	setAttrib(Shade, R_DimNamesSymbol, Shade_names);

	PROTECT(Hydraulic_flag = allocVector(LGLSXP, 4));
	LOGICAL_POINTER(Hydraulic_flag)[0] = v->grass.flagHydraulicRedistribution; //Grass
	LOGICAL_POINTER(Hydraulic_flag)[1] = v->shrub.flagHydraulicRedistribution; //Shrub
	LOGICAL_POINTER(Hydraulic_flag)[2] = v->tree.flagHydraulicRedistribution; //Tree
	LOGICAL_POINTER(Hydraulic_flag)[3] = v->forb.flagHydraulicRedistribution; //forb
	setAttrib(Hydraulic_flag, R_NamesSymbol, col_names);


	PROTECT(Hydraulic = allocMatrix(REALSXP, 3, 4));
	p_Hydraulic = REAL(Hydraulic);
	p_Hydraulic[0] = v->grass.maxCondroot;
	p_Hydraulic[1] = v->grass.swpMatric50;
	p_Hydraulic[2] = v->grass.shapeCond;
	p_Hydraulic[3] = v->shrub.maxCondroot;
	p_Hydraulic[4] = v->shrub.swpMatric50;
	p_Hydraulic[5] = v->shrub.shapeCond;
	p_Hydraulic[6] = v->tree.maxCondroot;
	p_Hydraulic[7] = v->tree.swpMatric50;
	p_Hydraulic[8] = v->tree.shapeCond;
	p_Hydraulic[9] = v->forb.maxCondroot;
	p_Hydraulic[10] = v->forb.swpMatric50;
	p_Hydraulic[11] = v->forb.shapeCond;
	PROTECT(Hydraulic_names = allocVector(VECSXP, 2));
	PROTECT(Hydraulic_names_x = allocVector(STRSXP,3));
	for (i = 0; i < 3; i++) {
		SET_STRING_ELT(Hydraulic_names_x, i, mkChar(cHydraulic_names[i]));
	}
	SET_VECTOR_ELT(Hydraulic_names,0,Hydraulic_names_x);
	SET_VECTOR_ELT(Hydraulic_names,1,col_names);
	setAttrib(Hydraulic, R_DimNamesSymbol, Hydraulic_names);

	PROTECT(CSWP = allocVector(REALSXP, 4));
	REAL(CSWP)[0] = v->grass.SWPcrit/-10; //Grass
	REAL(CSWP)[1] = v->shrub.SWPcrit/-10; //Shrub
	REAL(CSWP)[2] = v->tree.SWPcrit/-10; //Tree
	REAL(CSWP)[3] = v->forb.SWPcrit/-10; //Forb
	setAttrib(CSWP, R_NamesSymbol, col_names);

	PROTECT(MonthlyProductionValues_Column_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(MonthlyProductionValues_Column_names, i, mkChar(cMonthlyProductionValues_Column_names[i]));
	PROTECT(MonthlyProductionValues_Row_names = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++)
		SET_STRING_ELT(MonthlyProductionValues_Row_names, i, mkChar(cMonths[i]));

	PROTECT(Grasslands = allocMatrix(REALSXP, 12, 4));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		p_Grasslands[i + 12 * 0] = v->grass.litter[i];
		p_Grasslands[i + 12 * 1] = v->grass.biomass[i];
		p_Grasslands[i + 12 * 2] = v->grass.pct_live[i];
		p_Grasslands[i + 12 * 3] = v->grass.lai_conv[i];
	}
	PROTECT(Grasslands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Grasslands_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Grasslands_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Grasslands, R_DimNamesSymbol, Grasslands_names);

	PROTECT(Shrublands = allocMatrix(REALSXP, 12, 4));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		p_Shrublands[i + 12 * 0] = v->shrub.litter[i];
		p_Shrublands[i + 12 * 1] = v->shrub.biomass[i];
		p_Shrublands[i + 12 * 2] = v->shrub.pct_live[i];
		p_Shrublands[i + 12 * 3] = v->shrub.lai_conv[i];
	}
	PROTECT(Shrublands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Shrublands_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Shrublands_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Shrublands, R_DimNamesSymbol, Shrublands_names);

	PROTECT(Forest = allocMatrix(REALSXP, 12, 4));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		p_Forest[i + 12 * 0] = v->tree.litter[i];
		p_Forest[i + 12 * 1] = v->tree.biomass[i];
		p_Forest[i + 12 * 2] = v->tree.pct_live[i];
		p_Forest[i + 12 * 3] = v->tree.lai_conv[i];
	}
	PROTECT(Forest_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forest_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Forest_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Forest, R_DimNamesSymbol, Forest_names);

	PROTECT(Forb = allocMatrix(REALSXP, 12, 4));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		p_Forb[i + 12 * 0] = v->forb.litter[i];
		p_Forb[i + 12 * 1] = v->forb.biomass[i];
		p_Forb[i + 12 * 2] = v->forb.pct_live[i];
		p_Forb[i + 12 * 3] = v->forb.lai_conv[i];
	}
	PROTECT(Forb_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forb_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Forb_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Forb, R_DimNamesSymbol, Forb_names);

	SET_SLOT(VegProd, install(cVegProd_names[0]), VegComp);
	SET_SLOT(VegProd, install(cVegProd_names[1]), Albedo);
	SET_SLOT(VegProd, install(cVegProd_names[2]), conv_stcr);
	SET_SLOT(VegProd, install(cVegProd_names[3]), Canopy);
	SET_SLOT(VegProd, install(cVegProd_names[4]), VegInterception);
	SET_SLOT(VegProd, install(cVegProd_names[5]), LitterInterception);
	SET_SLOT(VegProd, install(cVegProd_names[6]), EsTpartitioning_param);
	SET_SLOT(VegProd, install(cVegProd_names[7]), Es_param_limit);
	SET_SLOT(VegProd, install(cVegProd_names[8]), Shade);
	SET_SLOT(VegProd, install(cVegProd_names[9]), Hydraulic_flag);
	SET_SLOT(VegProd, install(cVegProd_names[10]), Hydraulic);
	SET_SLOT(VegProd, install(cVegProd_names[11]), CSWP);
	SET_SLOT(VegProd, install(cVegProd_names[12]), Grasslands);
	SET_SLOT(VegProd, install(cVegProd_names[13]), Shrublands);
	SET_SLOT(VegProd, install(cVegProd_names[14]), Forest);
	SET_SLOT(VegProd, install(cVegProd_names[15]), Forb);

	UNPROTECT(36);
	return VegProd;
}
void onSet_SW_VPD(SEXP SW_VPD) {
	int i;
	SW_VEGPROD *v = &SW_VegProd;

	char *cVegProd_names[] = { "Composition", "Albedo", "Cover_stcr", "CanopyHeight", "VegetationInterceptionParameters", "LitterInterceptionParameters",
				"EsTpartitioning_param", "Es_param_limit", "Shade", "HydraulicRedistribution_use", "HydraulicRedistribution", "CriticalSoilWaterPotential",
				"MonthlyProductionValues_grass", "MonthlyProductionValues_shrub", "MonthlyProductionValues_tree", "MonthlyProductionValues_forb"};
	SEXP VegComp;
	SEXP Albedo;
	SEXP conv_stcr;
	SEXP Canopy;
	RealD *p_Canopy;
	SEXP VegInterception;
	RealD *p_VegInterception;
	SEXP LitterInterception;
	RealD *p_LitterInterception;
	SEXP EsTpartitioning_param;
	SEXP Es_param_limit;
	SEXP Shade;
	RealD *p_Shade;
	SEXP Hydraulic;
	SEXP Hydraulic_flag;
	SEXP CSWP;
	SEXP Grasslands;
	SEXP Shrublands;
	SEXP Forest;
	SEXP Forb;
	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;
	RealD fraction_sum = 0.;

	MyFileName = SW_F_name(eVegProd);

	PROTECT(VegComp = GET_SLOT(SW_VPD, install(cVegProd_names[0])));
	v->fractionGrass = REAL(VegComp)[0]; //Grass
	v->fractionShrub = REAL(VegComp)[1]; //Shrub
	v->fractionTree = REAL(VegComp)[2]; //Tree
	v->fractionForb = REAL(VegComp)[3]; //Forb
	v->fractionBareGround = REAL(VegComp)[4]; //Bare Ground

	PROTECT(Albedo = GET_SLOT(SW_VPD, install(cVegProd_names[1])));
	v->grass.albedo = REAL(Albedo)[0]; //Grass
	v->shrub.albedo = REAL(Albedo)[1]; //Shrub
	v->tree.albedo = REAL(Albedo)[2]; //Tree
	v->forb.albedo = REAL(Albedo)[3]; //Forb
	v->bareGround_albedo = REAL(Albedo)[4]; //Bare Ground

	PROTECT(conv_stcr = GET_SLOT(SW_VPD, install(cVegProd_names[2])));
	v->grass.conv_stcr = REAL(conv_stcr)[0]; //Grass
	v->shrub.conv_stcr = REAL(conv_stcr)[1]; //Shrub
	v->tree.conv_stcr = REAL(conv_stcr)[2]; //Tree
	v->forb.conv_stcr = REAL(conv_stcr)[3]; //Forb

	PROTECT(Canopy = GET_SLOT(SW_VPD, install(cVegProd_names[3])));
	p_Canopy = REAL(Canopy);
	v->grass.cnpy.xinflec = p_Canopy[0];
	v->grass.cnpy.yinflec = p_Canopy[1];
	v->grass.cnpy.range = p_Canopy[2];
	v->grass.cnpy.slope = p_Canopy[3];
	v->grass.canopy_height_constant = p_Canopy[4];
	v->shrub.cnpy.xinflec = p_Canopy[5];
	v->shrub.cnpy.yinflec = p_Canopy[6];
	v->shrub.cnpy.range = p_Canopy[7];
	v->shrub.cnpy.slope = p_Canopy[8];
	v->shrub.canopy_height_constant = p_Canopy[9];
	v->tree.cnpy.xinflec = p_Canopy[10];
	v->tree.cnpy.yinflec = p_Canopy[11];
	v->tree.cnpy.range = p_Canopy[12];
	v->tree.cnpy.slope = p_Canopy[13];
	v->tree.canopy_height_constant = p_Canopy[14];
	v->forb.cnpy.xinflec = p_Canopy[15];
	v->forb.cnpy.yinflec = p_Canopy[16];
	v->forb.cnpy.range = p_Canopy[17];
	v->forb.cnpy.slope = p_Canopy[18];
	v->forb.canopy_height_constant = p_Canopy[19];

	PROTECT(VegInterception = GET_SLOT(SW_VPD, install(cVegProd_names[4])));
	p_VegInterception = REAL(VegInterception);
	v->grass.veg_intPPT_a = p_VegInterception[0];
	v->grass.veg_intPPT_b = p_VegInterception[1];
	v->grass.veg_intPPT_c = p_VegInterception[2];
	v->grass.veg_intPPT_d = p_VegInterception[3];
	v->shrub.veg_intPPT_a = p_VegInterception[4];
	v->shrub.veg_intPPT_b = p_VegInterception[5];
	v->shrub.veg_intPPT_c = p_VegInterception[6];
	v->shrub.veg_intPPT_d = p_VegInterception[7];
	v->tree.veg_intPPT_a = p_VegInterception[8];
	v->tree.veg_intPPT_b = p_VegInterception[9];
	v->tree.veg_intPPT_c = p_VegInterception[10];
	v->tree.veg_intPPT_d = p_VegInterception[11];
	v->forb.veg_intPPT_a = p_VegInterception[12];
	v->forb.veg_intPPT_b = p_VegInterception[13];
	v->forb.veg_intPPT_c = p_VegInterception[14];
	v->forb.veg_intPPT_d = p_VegInterception[15];

	PROTECT(LitterInterception = GET_SLOT(SW_VPD, install(cVegProd_names[5])));
	p_LitterInterception = REAL(LitterInterception);
	v->grass.litt_intPPT_a = p_LitterInterception[0];
	v->grass.litt_intPPT_b = p_LitterInterception[1];
	v->grass.litt_intPPT_c = p_LitterInterception[2];
	v->grass.litt_intPPT_d = p_LitterInterception[3];
	v->shrub.litt_intPPT_a = p_LitterInterception[4];
	v->shrub.litt_intPPT_b = p_LitterInterception[5];
	v->shrub.litt_intPPT_c = p_LitterInterception[6];
	v->shrub.litt_intPPT_d = p_LitterInterception[7];
	v->tree.litt_intPPT_a = p_LitterInterception[8];
	v->tree.litt_intPPT_b = p_LitterInterception[9];
	v->tree.litt_intPPT_c = p_LitterInterception[10];
	v->tree.litt_intPPT_d = p_LitterInterception[11];
	v->forb.litt_intPPT_a = p_LitterInterception[12];
	v->forb.litt_intPPT_b = p_LitterInterception[13];
	v->forb.litt_intPPT_c = p_LitterInterception[14];
	v->forb.litt_intPPT_d = p_LitterInterception[15];

	PROTECT(EsTpartitioning_param = GET_SLOT(SW_VPD, install(cVegProd_names[6])));
	v->grass.EsTpartitioning_param = REAL(EsTpartitioning_param)[0]; //Grass
	v->shrub.EsTpartitioning_param = REAL(EsTpartitioning_param)[1]; //Shrub
	v->tree.EsTpartitioning_param = REAL(EsTpartitioning_param)[2]; //Tree
	v->forb.EsTpartitioning_param = REAL(EsTpartitioning_param)[3]; //Forb

	PROTECT(Es_param_limit = GET_SLOT(SW_VPD, install(cVegProd_names[7])));
	v->grass.Es_param_limit = REAL(Es_param_limit)[0]; //Grass
	v->shrub.Es_param_limit = REAL(Es_param_limit)[1]; //Shrub
	v->tree.Es_param_limit = REAL(Es_param_limit)[2]; //Tree
	v->forb.Es_param_limit = REAL(Es_param_limit)[3]; //Forb

	PROTECT(Shade = GET_SLOT(SW_VPD, install(cVegProd_names[8])));
	p_Shade = REAL(Shade);
	v->grass.shade_scale = p_Shade[0];
	v->grass.shade_deadmax = p_Shade[1];
	v->grass.tr_shade_effects.xinflec = p_Shade[2];
	v->grass.tr_shade_effects.yinflec = p_Shade[3];
	v->grass.tr_shade_effects.range = p_Shade[4];
	v->grass.tr_shade_effects.slope = p_Shade[5];
	v->shrub.shade_scale = p_Shade[6];
	v->shrub.shade_deadmax = p_Shade[7];
	v->shrub.tr_shade_effects.xinflec = p_Shade[8];
	v->shrub.tr_shade_effects.yinflec = p_Shade[9];
	v->shrub.tr_shade_effects.range = p_Shade[10];
	v->shrub.tr_shade_effects.slope = p_Shade[11];
	v->tree.shade_scale = p_Shade[12];
	v->tree.shade_deadmax = p_Shade[13];
	v->tree.tr_shade_effects.xinflec = p_Shade[14];
	v->tree.tr_shade_effects.yinflec = p_Shade[15];
	v->tree.tr_shade_effects.range = p_Shade[16];
	v->tree.tr_shade_effects.slope = p_Shade[17];
	v->forb.shade_scale = p_Shade[18];
	v->forb.shade_deadmax = p_Shade[19];
	v->forb.tr_shade_effects.xinflec = p_Shade[20];
	v->forb.tr_shade_effects.yinflec = p_Shade[21];
	v->forb.tr_shade_effects.range = p_Shade[22];
	v->forb.tr_shade_effects.slope = p_Shade[23];

	PROTECT(Hydraulic_flag = GET_SLOT(SW_VPD, install(cVegProd_names[9])));
	PROTECT(Hydraulic = GET_SLOT(SW_VPD, install(cVegProd_names[10])));
	v->grass.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[0]; //Grass
	v->shrub.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[1]; //Shrub
	v->tree.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[2]; //Tree
	v->forb.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[3]; //Forb
	v->grass.maxCondroot = REAL(Hydraulic)[0]; //Grass
	v->grass.swpMatric50 = REAL(Hydraulic)[1]; //Grass
	v->grass.shapeCond = REAL(Hydraulic)[2]; //Grass
	v->shrub.maxCondroot = REAL(Hydraulic)[3]; //Shrub
	v->shrub.swpMatric50 = REAL(Hydraulic)[4]; //Shrub
	v->shrub.shapeCond = REAL(Hydraulic)[5]; //Shrub
	v->tree.maxCondroot = REAL(Hydraulic)[6]; //Tree
	v->tree.swpMatric50 = REAL(Hydraulic)[7]; //Tree
	v->tree.shapeCond = REAL(Hydraulic)[8]; //Tree
	v->forb.maxCondroot = REAL(Hydraulic)[9]; //Forb
	v->forb.swpMatric50 = REAL(Hydraulic)[10]; //Forb
	v->forb.shapeCond = REAL(Hydraulic)[11]; //Forb

	PROTECT(CSWP =GET_SLOT(SW_VPD, install(cVegProd_names[11])));
	v->grass.SWPcrit = -10 * REAL(CSWP)[0]; //Grass
	v->shrub.SWPcrit = -10 * REAL(CSWP)[1]; //Shrub
	v->tree.SWPcrit = -10 * REAL(CSWP)[2]; //Tree
	v->forb.SWPcrit = -10 * REAL(CSWP)[3]; //Forb

	//PROTECT(MonthlyProductionValues = VECTOR_ELT(SW_VPD, 11));
	PROTECT(Grasslands = GET_SLOT(SW_VPD, install(cVegProd_names[12])));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		v->grass.litter[i] = p_Grasslands[i + 12 * 0];
		v->grass.biomass[i] = p_Grasslands[i + 12 * 1];
		v->grass.pct_live[i] = p_Grasslands[i + 12 * 2];
		v->grass.lai_conv[i] = p_Grasslands[i + 12 * 3];
	}
	PROTECT(Shrublands = GET_SLOT(SW_VPD, install(cVegProd_names[13])));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		v->shrub.litter[i] = p_Shrublands[i + 12 * 0];
		v->shrub.biomass[i] = p_Shrublands[i + 12 * 1];
		v->shrub.pct_live[i] = p_Shrublands[i + 12 * 2];
		v->shrub.lai_conv[i] = p_Shrublands[i + 12 * 3];
	}
	PROTECT(Forest = GET_SLOT(SW_VPD, install(cVegProd_names[14])));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		v->tree.litter[i] = p_Forest[i + 12 * 0];
		v->tree.biomass[i] = p_Forest[i + 12 * 1];
		v->tree.pct_live[i] = p_Forest[i + 12 * 2];
		v->tree.lai_conv[i] = p_Forest[i + 12 * 3];
	}
	PROTECT(Forb = GET_SLOT(SW_VPD, install(cVegProd_names[15])));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		v->forb.litter[i] = p_Forb[i + 12 * 0];
		v->forb.biomass[i] = p_Forb[i + 12 * 1];
		v->forb.pct_live[i] = p_Forb[i + 12 * 2];
		v->forb.lai_conv[i] = p_Forb[i + 12 * 3];
	}

	fraction_sum = v->fractionGrass + v->fractionShrub + v->fractionTree + v->fractionForb + v->fractionBareGround;
	if (!EQ(fraction_sum, 1.0)) {
			LogError(logfp, LOGWARN, "%s : Fractions of vegetation components were normalized, "
					"sum of fractions (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, fraction_sum);
			v->fractionGrass /= fraction_sum;
			v->fractionShrub /= fraction_sum;
			v->fractionTree /= fraction_sum;
			v->fractionBareGround /= fraction_sum;
			v->fractionForb /= fraction_sum;
			LogError(logfp, LOGWARN, "  Grassland fraction : %5.4f", v->fractionGrass);
			LogError(logfp, LOGWARN, "  Shrubland fraction : %5.4f", v->fractionShrub);
			LogError(logfp, LOGWARN, "  Forest/tree fraction : %5.4f", v->fractionTree);
			LogError(logfp, LOGWARN, "  FORB fraction : %5.4f", v->fractionForb);
			LogError(logfp, LOGWARN, "  Bare Ground fraction : %5.4f", v->fractionBareGround);
	}
	SW_VPD_init();

	if (EchoInits)
		_echo_inits();
	UNPROTECT(16);
}
#endif
void SW_VPD_construct(void) {
	/* =================================================== */

	memset(&SW_VegProd, 0, sizeof(SW_VegProd));

}

void SW_VPD_init(void) {
	/* ================================================== */
	/* set up vegetation parameters to be used in
	 * the "watrflow" subroutine.
	 *
	 * History:
	 *     Originally included in the FORTRAN model.
	 *
	 *     20-Oct-03 (cwb) removed the calculation of
	 *        lai_corr and changed the lai_conv value of 80
	 *        as found in the prod.in file.  The conversion
	 *        factor is now simply a divisor rather than
	 *        an equation.  Removed the following code:
	 lai_corr = v->lai_conv[m] * (1. - pstem) + aconst * pstem;
	 lai_standing    = v->biomass[m] / lai_corr;
	 where pstem = 0.3,
	 aconst = 464.0,
	 conv_stcr = 3.0;
	 *
	 *
	 */

	SW_VEGPROD *v = &SW_VegProd; /* convenience */
	TimeInt doy; /* base1 */

	if (GT(v->fractionGrass, 0.)) {
		interpolate_monthlyValues(v->grass.litter, v->grass.litter_daily);
		interpolate_monthlyValues(v->grass.biomass, v->grass.biomass_daily);
		interpolate_monthlyValues(v->grass.pct_live, v->grass.pct_live_daily);
		interpolate_monthlyValues(v->grass.lai_conv, v->grass.lai_conv_daily);
	}

	if (GT(v->fractionShrub, 0.)) {
		interpolate_monthlyValues(v->shrub.litter, v->shrub.litter_daily);
		interpolate_monthlyValues(v->shrub.biomass, v->shrub.biomass_daily);
		interpolate_monthlyValues(v->shrub.pct_live, v->shrub.pct_live_daily);
		interpolate_monthlyValues(v->shrub.lai_conv, v->shrub.lai_conv_daily);
	}

	if (GT(v->fractionTree, 0.)) {
		interpolate_monthlyValues(v->tree.litter, v->tree.litter_daily);
		interpolate_monthlyValues(v->tree.biomass, v->tree.biomass_daily);
		interpolate_monthlyValues(v->tree.pct_live, v->tree.pct_live_daily);
		interpolate_monthlyValues(v->tree.lai_conv, v->tree.lai_conv_daily);
	}

	if (GT(v->fractionForb, 0.)) {
		interpolate_monthlyValues(v->forb.litter, v->forb.litter_daily);
		interpolate_monthlyValues(v->forb.biomass, v->forb.biomass_daily);
		interpolate_monthlyValues(v->forb.pct_live, v->forb.pct_live_daily);
		interpolate_monthlyValues(v->forb.lai_conv, v->forb.lai_conv_daily);
	}

	for (doy = 1; doy <= MAX_DAYS; doy++) {
		if (GT(v->fractionGrass, 0.)) {
			lai_standing = v->grass.biomass_daily[doy] / v->grass.lai_conv_daily[doy];
			v->grass.pct_cover_daily[doy] = lai_standing / v->grass.conv_stcr;
			if (GT(v->grass.canopy_height_constant, 0.)) {
				v->grass.veg_height_daily[doy] = v->grass.canopy_height_constant;
			} else {
				v->grass.veg_height_daily[doy] = tanfunc(v->grass.biomass_daily[doy],
						v->grass.cnpy.xinflec,
						v->grass.cnpy.yinflec,
						v->grass.cnpy.range,
						v->grass.cnpy.slope); /* used for vegcov and for snowdepth_scale */
			}
			v->grass.lai_live_daily[doy] = lai_standing * v->grass.pct_live_daily[doy];
			v->grass.vegcov_daily[doy] = v->grass.pct_cover_daily[doy] * v->grass.veg_height_daily[doy]; /* used for vegetation interception */
			v->grass.biolive_daily[doy] = v->grass.biomass_daily[doy] * v->grass.pct_live_daily[doy];
			v->grass.biodead_daily[doy] = v->grass.biomass_daily[doy] - v->grass.biolive_daily[doy]; /* used for transpiration */
			v->grass.total_agb_daily[doy] = v->grass.litter_daily[doy] + v->grass.biomass_daily[doy]; /* used for bare-soil evaporation */
		} else {
			v->grass.lai_live_daily[doy] = 0.;
			v->grass.vegcov_daily[doy] = 0.;
			v->grass.biolive_daily[doy] = 0.;
			v->grass.biodead_daily[doy] = 0.;
			v->grass.total_agb_daily[doy] = 0.;
		}

		if (GT(v->fractionShrub, 0.)) {
			lai_standing = v->shrub.biomass_daily[doy] / v->shrub.lai_conv_daily[doy];
			v->shrub.pct_cover_daily[doy] = lai_standing / v->shrub.conv_stcr;
			if (GT(v->shrub.canopy_height_constant, 0.)) {
				v->shrub.veg_height_daily[doy] = v->shrub.canopy_height_constant;
			} else {
				v->shrub.veg_height_daily[doy] = tanfunc(v->shrub.biomass_daily[doy],
						v->shrub.cnpy.xinflec,
						v->shrub.cnpy.yinflec,
						v->shrub.cnpy.range,
						v->shrub.cnpy.slope); /* used for vegcov and for snowdepth_scale */
			}
			v->shrub.lai_live_daily[doy] = lai_standing * v->shrub.pct_live_daily[doy];
			v->shrub.vegcov_daily[doy] = v->shrub.pct_cover_daily[doy] * v->shrub.veg_height_daily[doy]; /* used for vegetation interception */
			v->shrub.biolive_daily[doy] = v->shrub.biomass_daily[doy] * v->shrub.pct_live_daily[doy];
			v->shrub.biodead_daily[doy] = v->shrub.biomass_daily[doy] - v->shrub.biolive_daily[doy]; /* used for transpiration */
			v->shrub.total_agb_daily[doy] = v->shrub.litter_daily[doy] + v->shrub.biomass_daily[doy]; /* used for bare-soil evaporation */
		} else {
			v->shrub.lai_live_daily[doy] = 0.;
			v->shrub.vegcov_daily[doy] = 0.;
			v->shrub.biolive_daily[doy] = 0.;
			v->shrub.biodead_daily[doy] = 0.;
			v->shrub.total_agb_daily[doy] = 0.;
		}

		if (GT(v->fractionTree, 0.)) {
			lai_standing = v->tree.biomass_daily[doy] / v->tree.lai_conv_daily[doy];
			v->tree.pct_cover_daily[doy] = lai_standing / v->tree.conv_stcr;
			if (GT(v->tree.canopy_height_constant, 0.)) {
				v->tree.veg_height_daily[doy] = v->tree.canopy_height_constant;
			} else {
				v->tree.veg_height_daily[doy] = tanfunc(v->tree.biomass_daily[doy],
						v->tree.cnpy.xinflec,
						v->tree.cnpy.yinflec,
						v->tree.cnpy.range,
						v->tree.cnpy.slope); /* used for vegcov and for snowdepth_scale */
			}
			v->tree.lai_live_daily[doy] = lai_standing * v->tree.pct_live_daily[doy]; /* used for vegetation interception */
			v->tree.vegcov_daily[doy] = v->tree.pct_cover_daily[doy] * v->tree.veg_height_daily[doy];
			v->tree.biolive_daily[doy] = v->tree.biomass_daily[doy] * v->tree.pct_live_daily[doy];
			v->tree.biodead_daily[doy] = v->tree.biomass_daily[doy] - v->tree.biolive_daily[doy]; /* used for transpiration */
			v->tree.total_agb_daily[doy] = v->tree.litter_daily[doy] + v->tree.biolive_daily[doy]; /* used for bare-soil evaporation */
		} else {
			v->tree.lai_live_daily[doy] = 0.;
			v->tree.vegcov_daily[doy] = 0.;
			v->tree.biolive_daily[doy] = 0.;
			v->tree.biodead_daily[doy] = 0.;
			v->tree.total_agb_daily[doy] = 0.;
		}

		if (GT(v->fractionForb, 0.)) {
			lai_standing = v->forb.biomass_daily[doy] / v->forb.lai_conv_daily[doy];
			v->forb.pct_cover_daily[doy] = lai_standing / v->forb.conv_stcr;
			if (GT(v->forb.canopy_height_constant, 0.)) {
				v->forb.veg_height_daily[doy] = v->forb.canopy_height_constant;
			} else {
				v->forb.veg_height_daily[doy] = tanfunc(v->forb.biomass_daily[doy],
						v->forb.cnpy.xinflec,
						v->forb.cnpy.yinflec,
						v->forb.cnpy.range,
						v->forb.cnpy.slope); /* used for vegcov and for snowdepth_scale */
			}
			v->forb.lai_live_daily[doy] = lai_standing * v->forb.pct_live_daily[doy]; /* used for vegetation interception */
			v->forb.vegcov_daily[doy] = v->forb.pct_cover_daily[doy] * v->forb.veg_height_daily[doy];
			v->forb.biolive_daily[doy] = v->forb.biomass_daily[doy] * v->forb.pct_live_daily[doy];
			v->forb.biodead_daily[doy] = v->forb.biomass_daily[doy] - v->forb.biolive_daily[doy]; /* used for transpiration */
			v->forb.total_agb_daily[doy] = v->forb.litter_daily[doy] + v->forb.biolive_daily[doy]; /* used for bare-soil evaporation */
		} else {
			v->forb.lai_live_daily[doy] = 0.;
			v->forb.vegcov_daily[doy] = 0.;
			v->forb.biolive_daily[doy] = 0.;
			v->forb.biodead_daily[doy] = 0.;
			v->forb.total_agb_daily[doy] = 0.;
		}
	}
}

static void _echo_inits(void) {
	/* ================================================== */

	SW_VEGPROD *v = &SW_VegProd; /* convenience */
	char outstr[1500];

	sprintf(errstr, "\n==============================================\n"
			"Vegetation Production Parameters\n\n");
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);

	sprintf(errstr, "Grassland component\t= %1.2f\n"
			"\tAlbedo\t= %1.2f\n"
			"\tHydraulic redistribution flag\t= %d\n", v->fractionGrass, v->grass.albedo, v->grass.flagHydraulicRedistribution);
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);

	sprintf(errstr, "Shrubland component\t= %1.2f\n"
			"\tAlbedo\t= %1.2f\n"
			"\tHydraulic redistribution flag\t= %d\n", v->fractionShrub, v->shrub.albedo, v->shrub.flagHydraulicRedistribution);
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);

	sprintf(errstr, "Forest-Tree component\t= %1.2f\n"
			"\tAlbedo\t= %1.2f\n"
			"\tHydraulic redistribution flag\t= %d\n", v->fractionTree, v->tree.albedo, v->tree.flagHydraulicRedistribution);
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);

	sprintf(errstr, "FORB component\t= %1.2f\n"
			"\tAlbedo\t= %1.2f\n"
			"\tHydraulic redistribution flag\t= %d\n", v->fractionForb, v->forb.albedo, v->forb.flagHydraulicRedistribution);
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);

	sprintf(errstr, "Bare Ground component\t= %1.2f\n"
			"\tAlbedo\t= %1.2f\n", v->fractionBareGround, v->bareGround_albedo);
	strcpy(outstr, errstr);
	LogError(logfp, LOGNOTE, outstr);
}
