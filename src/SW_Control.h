/********************************************************/
/********************************************************/
/*  Source file: SW_Control.h
 *  Type: header
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: This module controls the flow of the model.
 *           Previously this was done in main() but to
 *           combine the model with other code (eg STEPPE)
 *           there needs to be separate callable routines
 *           for initializing, model flow, and output.
 *
 *  History:
 *     (10-May-02) -- INITIAL CODING - cwb
 */
/********************************************************/
/********************************************************/

#ifndef SW_CONTROL_H
#define SW_CONTROL_H

void SW_CTL_init_model(const char *firstfile);
void SW_CTL_main(void); /* main controlling loop for SOILWAT  */
void SW_CTL_run_current_year(void);

#ifdef DEBUG_MEM
void SW_CTL_SetMemoryRefs(void);
#endif

#endif
