/* myMemory.c  - collection of routines to handle memory
 * management beyond malloc() and free().
 *
 * Some of the code comes from "Writing Solid Code" by
 * Steve Maguire, 1993 (Microsoft Press), and some comes
 * from "A Memory Controller" by Robert A. Moeser, (Dr.
 * Dobbs Journal, 1990, v164).
 *
 * The basic idea is to add bookkeeping information from
 * allocation requests to provide feedback regarding typical
 * memory management problems such as
 - using (dereferencing) pointers to memory that has already
 been freed,
 - dereferencing pointers before/without allocating their
 memory,
 - dereferencing pointers outside the allocated memory block.

 * Some of the code is mine.  I added a define called DEBUG_MEM
 * to distinguish this and other types of debugging, such as
 * other algorithms, etc.  However, the other code makes heavy
 * use of the standard assert macro which relies on the DEBUG
 * define.  So in some places you might see DEBUG or DEBUG_MEM
 * depending on the context.
 * IF 'DEBUG_MEM' IS DEFINED, MAKE SURE 'DEBUG' IS DEFINED ALSO.

 * - CWBennett 7/17/01 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "generic.h"
#include "myMemory.h"

/*  not sure how to handle this block migrated from gen_funcs.c */
#ifdef DEBUG_MEM_X
struct mem_debug_st {
	size_t handle, size;
	struct mem_debug_st *next;
}*Mem=NULL;
size_t SizeOfMalloc=0;
struct mem_debug_st * mem_GetNode( void *handle);
size_t mem_SizeOf( void *handle);
void mem_AddNode( void *block, size_t size);
void mem_DelNode( void *handle);

#endif

/* Note that errstr[] is externed via generic.h */

/*****************************************************/
char *Str_Dup(const char *s) {
	/*-------------------------------------------
	 Duplicate a string s by allocating memory for
	 it, copying s to the new location and
	 returning a pointer to the new string.

	 Provides error handling by failing.

	 cwb - 9/13/01

	 -------------------------------------------*/

	char *p;

	p = (char *) Mem_Malloc(strlen(s) + 1, "Str_Dup()");

	strcpy(p, s);

	return p;

}

/*****************************************************/
void *Mem_Malloc(size_t size, const char *funcname) {
	/*-------------------------------------------
	 Provide a generic malloc that tests for failure.

	 cwb - 6/27/00
	 5/19/01 - added debugging code for memory checking
	 - added Mem_Set() for default behavior
	 7/23/01 - added code from Macguire to install
	 memory logging functions.  Modified
	 the Mem_Set call to fit his suggestions.
	 Changed the names to Mem_* to be more
	 consistent with other modules.
	 -------------------------------------------*/

	void *p;

#ifdef DEBUG_MEM_LOG
	FILE *f;
#endif

#ifdef DEBUG_MEM
	{
		if (size == 0)
		LogError(stderr, LOGFATAL,"Programmer Error: "
				"size == 0 in MallocErr()");

	}
#endif

	p = malloc(size);

#ifdef DEBUG_MEM_LOG
	if( NULL==(f=fopen("memory.log","a")) ) {
#ifndef RSOILWAT
		fprintf(stderr, "Can't open memory.log for errors\n");
		exit(-1);
#else
		Rprintf("Can't open memory.log for errors\n");
		error("mymemory.c NULL==(f=fopen(");
#endif
	}
#ifndef RSOILWAT
	fprintf(f,"%s: %d: %p\n", funcname, size, p);
	fclose(f);
#else
	Rprintf("%s: %d: %p\n", funcname, size, p);
#endif
#endif

	if (p == NULL )
		LogError(stderr, LOGFATAL, "Out of memory in %s()", funcname);

#ifdef DEBUG_MEM
	{
		/* can't use Mem_Set() yet because no block info.
		 * however, the real malloc() just succeeded, so
		 * we should be able to assume integrity (?) */
		memset(p, (int)bGarbage, size);

		/* if unable to create the block information,
		 * fake a total memory failure.
		 */
		if (!fCreateBlockInfo(p, size)) {
			free( p); /* same argument as for memset() */
			p = NULL;
		}

	}
#endif

#ifdef DEBUG_MEM_X
	if (p) mem_AddNode(p, size);
#endif

	return p;

}

/*****************************************************/
void *Mem_Calloc(size_t nobjs, size_t size, const char *funcname) {
	/*-------------------------------------------
	 Duplicates the behavior of calloc() similar to
	 Mem_Malloc.

	 cwb - 8/10/01
	 -------------------------------------------*/

	void *p;

	p = Mem_Malloc(size * nobjs, funcname);

#ifndef DEBUG_MEM
	/* if using mcguire's code, no need to memset after
	 * malloc() as it is done there.
	 */
	Mem_Set(p, 0, size * nobjs);
#endif

	return p;

}

/*****************************************************/
void *Mem_ReAlloc(void *block, size_t sizeNew) {
	/*-------------------------------------------
	 Provide a wrapper for realloc() that facilitates debugging.
	 Copied from Macguire.

	 Normally, realloc() can possibly move the reallocated block
	 resulting in potentially orphaned pointers somewhere in the
	 code.  The debug block here forces the new block to be in a
	 different place every time, allowing such bugs to be
	 triggered.

	 cwb - 7/23/2001
	 -------------------------------------------*/
	byte *p = (byte *) block, /* a copy so as not to damage original ? */
	*pNew;
#ifdef DEBUG_MEM
	size_t sizeOld;
#endif

#ifndef RSOILWAT
	assert(p != NULL && sizeNew > 0);
#else
	if(p == NULL || sizeNew == 0)
		error("assert failed in ReAlloc");
#endif

#ifdef DEBUG_MEM
	{
		sizeOld = sizeofBlock(p);
		/* if the block is shrinking, pre-fill the soon-to-be-
		 * released memory.  If the block is expanding, force
		 * it to move (instead of expanding in place) by
		 * faking a realloc. If the block is the same size,
		 * don't do anything.
		 */

		if (sizeNew < sizeOld)
		memset((p+sizeNew), bGarbage, sizeOld - sizeNew);
		else if (sizeNew > sizeOld) {
			byte *pForceNew;
			if (NULL != (pForceNew = (byte *)Mem_Malloc( sizeNew,"Mem_Realloc()"))) {
				memcpy( pForceNew, p, sizeOld);
				Mem_Free( p);
				p = pForceNew;
			}
		}
	}
#endif

	pNew = (byte *) realloc(p, sizeNew);

	if (pNew != NULL ) {
#ifdef DEBUG_MEM
		{
			UpdateBlockInfo(p, pNew, sizeNew);

			/* if expanding, initialize the new tail */
			if (sizeNew > sizeOld)
			memset(pNew+sizeOld, bGarbage, sizeNew - sizeOld);
		}
#endif

		p = pNew;
	} else
		LogError(stderr, LOGFATAL, "realloc failed in Mem_ReAlloc()");

	return p;
}

/*****************************************************/
void Mem_Free(void *block) {
	/*-------------------------------------------
	 Provide a wrapper for free() that facilitates debugging.

	 cwb - 5/19/2001
	 7/23/01  - added Macguire's code.
	 -------------------------------------------*/

#ifdef DEBUG_MEM_X
	{
		if (mem_SizeOf(block) > SizeOfMalloc)
		LogError(stderr, LOGFATAL,"Mem: Inconsistency in SizeOfMalloc");

		mem_DelNode(block);
	}
#endif

#ifdef DEBUG_MEM
	{
		assert(fValidPointer(block, sizeofBlock(block)));
		memset( block, bGarbage, sizeofBlock(block));
		FreeBlockInfo(block);
	}
#endif

	free(block);

}

/*****************************************************/
void Mem_Set(void *block, byte c, size_t n) {
	/*-------------------------------------------
	 Provide a wrapper for memset() that facilitates debugging.

	 cwb - 5/21/2001
	 7/23/01  - added Macguire's code.
	 -------------------------------------------*/

#ifdef DEBUG_MEM
#ifndef RSOILWAT
	assert(fValidPointer(block, n));
#endif
#endif

	memset(block, (int) c, n);

}

/*****************************************************/
void Mem_Copy(void *dest, const void *src, size_t n) {
	/*-------------------------------------------
	 Provide a wrapper for memcpy() that facilitates debugging.

	 cwb - 7/23/01  - added Macguire's code.
	 -------------------------------------------*/

#ifdef DEBUG_MEM
	{
#ifndef RSOILWAT
		assert(fValidPointer((byte *)dest, n));
		assert(fValidPointer((byte *)src, n));
		if (fPtrLess( ((byte *)src), (byte *)dest) )
		assert(fPtrLess( ((byte *)src+n), (byte *)dest));
		else
		assert(fPtrGrtr( ((byte *)src+1), (byte *)dest));
#endif
	}
#endif

	memcpy(dest, src, n);

}

/* ===============  end of block from gen_funcs.c ----------------- */
/* ================ see also the end of this file ------------------ */

#ifdef DEBUG_MEM

/*================================================================*/
/* This section contains the memory logging routines from
 * Appendix B in Writing Solid Code.  Here are the preliminary
 * comments copied  from the book.
 "  The code in this appendix implements a simple linked-list
 version of the memory logging routines that are discussed
 in Chapter 3.  The code is intentionally simple so that it
 can be easily understood--is is not meant to be used in any
 application that makes heavy use of the memory manager.  But
 before you spend time rewriting the routines to use an
 AVL-tree, a B-tree, or any other data structure that provides
 fast searches, first try the code to  verify that it is indeed
 to slow for practical use in your application. You may find
 that the code works well for you as is, particularly if you
 don't maintain many globally allocated memory blocks.
 "  The implementation in this file is straightforward: For every
 allocated memory block, these routines allocate an extra bit
 of memory to hold a blockinfo structure that contains the log
 information.  See its definition below.  When a new blockinfo
 structure is created, it is filled in and placed at the head
 of the linked-list structure--there is no attempt to maintain
 any particular ordering for the list. Again, this implementation
 was chosen because it is simple and easy to understand.
 ----------------------------------------------------------------*/

/* See memblock.h */

/* -------------------------------------------------------------- *
 *     - * - * - *  Private Functions  * - * - * -
 * -------------------------------------------------------------- */

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * pbiHead points to a singly linked list of debugging information
 * for the memory manager.
 */
static blockinfo *pbiHead = NULL;

/* --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * pbiGetBlockInfo searches the memory log to find the block
 * that pb points into and returns a pointer to the
 * corresponding blockinfo structure of the memory log.
 * Note: pb *must* point into an allocated block or you
 * will get an assertion failure; the function either
 * asserts or succeeds -- it never returns an error.
 *
 *  blockinfo *pbi;
 *  ...
 *  pbi = pbiGetBlockInfo(pb);
 *  // pbi->pb points to the start of pb's block
 *  // pbi->size is the size of the block that pb points to.
 */

static blockinfo *pbiGetBlockInfo(byte *pb) {

	blockinfo *pbi;

	for (pbi = pbiHead; pbi != NULL; pbi = pbi->pbiNext) {
		byte *pbStart = pbi->pb; /* for readability */
		byte *pbEnd = pbi->pb + pbi->size - 1;

		if (fPtrGrtrEq(pb, pbStart) && fPtrLessEq(pb, pbEnd))
		break;
	}

	/* Couldn't find pointer?  Is it (a) garbage? (b) pointing
	 * to a block that was freed? or (c) pointing to a block
	 * that moved when it was resized by fResizeMemory?
	 */
#ifndef RSOILWAT
	assert(pbi != NULL);
#endif
	return (pbi);
}

/* -------------------------------------------------------------- *
 *      *  *  *  Public Functions  *  *  *
 * -------------------------------------------------------------- */

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * fCreateBlockInfo(pbNew, sizeNew)
 *
 * This function creates a log entry for the memory block
 * defined by pbNew:sizeNew.  The function returns TRUE if it
 * successfully creates the log information; FALSE otherwise.
 *
 *    if (fCreateBlockInfo(pbNew, sizeNew))
 *      //success -- the memory log has an entry
 *    else
 *      // failure -- no entry, so release pbNew
 */

flag fCreateBlockInfo(byte *pbNew, size_t sizeNew) {

	blockinfo *pbi;
#ifndef RSOILWAT
	assert(pbNew != NULL && sizeNew > 0);
#endif
	pbi = (blockinfo *)malloc(sizeof(blockinfo));
	if (pbi != NULL) {
		pbi->pb = pbNew;
		pbi->size = sizeNew;
		pbi->pbiNext = pbiHead;
		pbiHead = pbi;
	}

	return (flag)(pbi != NULL);
}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * FreeBlockInfo(pbToFree)
 *
 * This function destroys the log entry for the memory block
 * that pbToFree points to.  pbToFree *must* point to the start
 * of an allocated block; otherwise you will get an assertion
 * failure.
 */

void FreeBlockInfo( byte *pbToFree) {

	blockinfo *pbi, *pbiPrev;

	pbiPrev = NULL;

	for (pbi = pbiHead; pbi != NULL; pbi = pbi->pbiNext) {
		if (fPtrEqual(pbi->pb, pbToFree)) {
			if (pbiPrev == NULL)
			pbiHead = pbi->pbiNext;
			else
			pbiPrev->pbiNext = pbi->pbiNext;

			break;
		}
		pbiPrev = pbi;
	}
#ifndef RSOILWAT
	/* If pbi is NULL, then pbiToFree is invalid */
	assert(pbi != NULL);
#endif
	/* Destroy the contents of *pbi before freeing them */
	memset(pbi, bGarbage, sizeof(blockinfo));

	free(pbi);
}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * UpdateBlockInfo(pbOld, pbNew, sizeNew)
 *
 * Looks up the log information for the memory block that
 * pbOld points to.  The function then updates the log information
 * to reflect the fact that the block new lives at pbNew and
 * is "sizeNew bytes" long.  pbOld *must* point to the start of
 * an allocated block; otherwise, you will get an assertion
 * failure.
 */

void UpdateBlockInfo(byte *pbOld, byte *pbNew, size_t sizeNew) {

	blockinfo *pbi;
#ifndef RSOILWAT
	assert(pbNew != NULL && sizeNew > 0);
#endif
	pbi = pbiGetBlockInfo(pbOld);
#ifndef RSOILWAT
	assert(pbOld == pbi->pb);
#endif
	pbi->pb = pbNew;
	pbi->size = sizeNew;
}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * sizeofBlock(pb)
 *
 * sizeofBlock returns the size of the block that pb points to.
 * pb *must* point to the start of an allocated block;
 * otherwise, you will get an assertion failure.
 */

size_t sizeofBlock(byte *pb) {

	blockinfo *pbi;

	pbi = pbiGetBlockInfo(pb);
#ifndef RSOILWAT
	assert(pb == pbi->pb);
#endif
	return (pbi->size);
}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * The following routines are used to find dangling
 * pointers and lost memory blocks.  See Chapter 3
 * for a discussion of these routines.
 * --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- */

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * ClearMemoryRefs(void)
 *
 * Mark all blocks in the memory log as being unreferenced.
 */

void ClearMemoryRefs(void) {

	blockinfo *pbi;
	int i = 0;
	for ( pbi = pbiHead; pbi != NULL; pbi = pbi->pbiNext) {
		pbi->fReferenced = FALSE;
		i++;
	}

}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * NoteMemoryRef(pv)
 *
 * Marks the block that pv points into as being referenced.
 * Note: pv does *not* have to point to the start of a block;
 * it may point anywhere within an allocated block.
 */
void NoteMemoryRef(void *pv) {

	blockinfo *pbi;

	pbi = pbiGetBlockInfo((byte *)pv);
	pbi->fReferenced = TRUE;

}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * CheckMemoryRefs(void)
 *
 * Scans the memory log looking for blocks that have not been
 * marked with a call to NoteMemoryRef.  If this function finds
 * an unmarked block, it asserts.
 */
void CheckMemoryRefs(void) {

	blockinfo *pbi;
	int i=0;

	for (pbi = pbiHead; pbi != NULL; pbi = pbi->pbiNext) {
		/* a simple check for block integrity.  If this
		 * assert fires, it meanse that something is wrong
		 * with the debug code that manages blockinfo or,
		 * possibly, that a wild memory store has trashed the
		 * data structure.  Either way, it's a bug.
		 */
#ifndef RSOILWAT
		assert(pbi->pb != NULL && pbi->size > 0);
#endif
		/* printf("i=%d, size=%d, p=%p\n", ++i, pbi->size, pbi->pb); */

		/* A check for lost or leaky memory.  if this assert
		 * fires, it means that the app has either lost track
		 * of this block or that not all global pointers have
		 * been accounted for with NoteMemoryRef.
		 */
#ifndef RSOILWAT
		assert(pbi->fReferenced);
#endif
	}
}

/* --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- *
 * fValidPointer(pv, size)
 *
 * fValidPointer verifies that pv points into an allocated
 * memory block and that there are at least "size" allocated
 * bytes from pv to the end of the block. If either condition
 * is not met fValidPointer will assert; the function will
 * never return false.
 *
 * The reason fValidPointer returns a flag at all (always TRUE)
 * is to allow you to call the function within an assert macro.
 * While this isn't the most efficient method to use, using the
 * macro neatly handles the debug-vs-ship version control
 * issue without your having to resort to #ifdef DEBUG's or
 * to introducing other assert-like macros.
 *
 *   assert(fValidPointer(pb, size));
 */
flag fValidPointer(void *pv, size_t size) {

	blockinfo *pbi;
	byte *pb = (byte *)pv;
#ifndef RSOILWAT
	assert(pv != NULL && size > 0);
#endif
	pbi = pbiGetBlockInfo(pb); /* this validates pv */
#ifndef RSOILWAT
	/* size isn't valid if pb+size overflows the block */
	assert(fPtrLessEq(pb+size, pbi->pb + pbi->size));
#endif
	return(TRUE);
}

#endif

