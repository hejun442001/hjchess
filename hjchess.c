/*
 *       HE Jun's simple chess program
 */

#define VERSION "V3.20160315.1"
#define MAX_PLY (1000)
#define OPENING_BOOK_FILENAME "openbook.txt"
#define INF (0x7FFF)
#define UNUSED(x) (x)=(x);
/*
 *    data strustures
 */
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
typedef unsigned char BYTE;
typedef unsigned long long int U64;
#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define DIFF(a,b) ((a)>(b) ? (a)-(b) : (b)-(a))

unsigned long ply,init_ply;         /* Number of move */
#define WTM ((~ply) & 1)  /* White-to-move */

BYTE castle_sq[64]= {0}; /* Which pieces may participate in castling */
#define CASTLE_WHITE_SHORT 1
#define CASTLE_BLACK_SHORT 2
#define CASTLE_WHITE_LONG  4
#define CASTLE_BLACK_LONG  8
#define CASTLE_WHITW_DONE  16
#define CASTLE_BLACK_DONE  32

struct side
{
    BYTE attack[64];
    BYTE king;
    BYTE pawn[10];
    int mat;
} white, black, *self, *opp, *stronger, *weaker;

#define DRAWN_VALUE (0)

BYTE captures[13];
BYTE caps[2];

BYTE computer[3];

int pv[MAX_PLY][MAX_PLY];
unsigned long pv_rear[MAX_PLY];

signed char undo_stack[10*MAX_PLY], *undo_sp;
signed char undo_stack[10*MAX_PLY], *undo_sp;
U64 hash_arr[2048];

int maxdepth   =  MAX_PLY;
unsigned long time_limit = 60*CLOCKS_PER_SEC;

struct move
{
    short move;
    unsigned int prescore;
} move_stack[35*MAX_PLY], *move_sp;

int piece_square[2][13][64]; /* piece/square table*/
U64 zobrist[13][64];  /* Used for hash-key */
int histroy[64*64];
int killer_move[MAX_PLY][2];

#define CORE (16*1024*1024)
long book_size;    /* Number of opening book entries */
struct mem
{
    U64 hash;       /* identifies position */
    unsigned short move;     /* move */
    short score;    /* score */
    BYTE flag;      /* flag */
    BYTE depth;    /* remain search depth */
} core[CORE];
#define TTFLAG_BOOK     0
#define TTFLAG_BETA     1
#define TTFLAG_PV       2
#define TTFLAG_NON_PV   3
#define TTFLAG_ALPHA    4

/* Constants for  move ordering (pre-scores) */
#define PRESCORE_PV_MOVE        (0x8000)
#define PRESCORE_TTABLE_BEST    (0x4000)
#define PRESCORE_KILLER_MOVE    (0x2000)
#define PRESCORE_BASE           (0x1000)
#define PRESCORE_CAPTURES       (0x0800)
/*
 *    Chess defines
 */
int board[64+3];
int last_src;           /* Last from square */
int last_dest;          /* Last target square */
/* board - special squares */
#define CASTLE      64      /* Castle rights */
#define EP          65      /* En passant */
#define RULE50      66      /* 50 moves rule counter */
/* board - useful squares */
#define A1      0
#define A8      7
#define B1      8
#define B8      15
#define C1      16
#define C8      23
#define D1      24
#define D8      31
#define E1      32
#define E8      39
#define F1      40
#define F8      47
#define G1      48
#define G8      55
#define H1      56
#define H8      63
/* board - files */
#define FILE_A                  0
#define FILE_B                  1
#define FILE_C                  2
#define FILE_D                  3
#define FILE_E                  4
#define FILE_F                  5
#define FILE_G                  6
#define FILE_H                  7
/* board - ranks */
#define RANK_1                  0
#define RANK_2                  1
#define RANK_3                  2
#define RANK_4                  3
#define RANK_5                  4
#define RANK_6                  5
#define RANK_7                  6
#define RANK_8                  7

/* pieces */
#define EMPTY                   0
#define WHITE_KING              1
#define WHITE_QUEEN             2
#define WHITE_ROOK              3
#define WHITE_BISHOP            4
#define WHITE_KNIGHT            5
#define WHITE_PAWN              6
#define BLACK_KING              7
#define BLACK_QUEEN             8
#define BLACK_ROOK              9
#define BLACK_BISHOP            10
#define BLACK_KNIGHT            11
#define BLACK_PAWN              12

#define PIECE_COLOR(pc) ((pc) < BLACK_KING?1:0) /* White or black */

#define OFFSET_N       (+1)        /* Offset to step one square up (north) */
#define OFFSET_E       (+8)        /* Offset to step one square right (east) */

#define RANK2CHAR(r)            ('1'+(r))
#define CHAR2RANK(c)            ((c)-'1')
#define FILE2CHAR(f)            ('a'+(f))
#define CHAR2FILE(c)            ((c)-'a')
#define PIECE2CHAR(p)           (".KQRBNPkqrbnp"[p])

#define F(square)               (((square) >> 3)&7)     /* file */
#define R(square)               ((square) & 7)          /* rank */
#define SQ(f,r)                 (((f&7) << 3)|(r&7))    /* compose square */
#define FLIP(square)            ((square)^7)            /* flip board */
#define MOVE(fr,to)             (((fr&077) << 6)|(to&077))/* compose move */
#define FR(move)                (((move) & 07700) >> 6) /* from square */
#define TO(move)                ((move) & 00077)        /* target square */
#define SPECIAL                 (1<<12)                 /* special moves mask*/

const int init_piece_num[]=
{
    0,
    1, 1, 2, 2, 2, 8,
    1, 1, 2, 2, 2, 8,
};
int piece_counter[13];

/*
 *      Game phases
 */

int phase (void)
{
    const int KnightPhase = 1;
    const int BishopPhase = 1;
    const int RookPhase = 2;
    const int QueenPhase = 4;
    const int TotalPhase =  KnightPhase*4 + BishopPhase*4 + RookPhase*4 + QueenPhase*2;
    int phase = TotalPhase
                -(piece_counter[WHITE_QUEEN]+piece_counter[BLACK_QUEEN])*QueenPhase
                -(piece_counter[WHITE_ROOK]+piece_counter[BLACK_ROOK])*RookPhase
                -(piece_counter[WHITE_KNIGHT]+piece_counter[BLACK_KNIGHT])*KnightPhase
                -(piece_counter[WHITE_BISHOP]+piece_counter[BLACK_BISHOP])*BishopPhase;
    return (phase * 256 + (TotalPhase / 2)) / TotalPhase;
}
int tapered(int MG, int EG)
{
    int ph = phase();
    return ((MG * (256 - ph)) + (EG * ph)) / 256;
}

int MidgameLimit  = 15581, EndgameLimit  = 3998;
int ending(void)
{
    return black.mat + white.mat <= EndgameLimit;
}
int opening(void)
{
    return  black.mat + white.mat > MidgameLimit;
}

/* attacks */
#define DISTANCE(a,b) (DIFF(F(a),F(b)) + DIFF(R(a),R(b)))
signed short king_step[8]=
{
    +OFFSET_E, +OFFSET_N,
    -OFFSET_E, -OFFSET_N,
    +OFFSET_E+OFFSET_N, +OFFSET_E-OFFSET_N,
    -OFFSET_E+OFFSET_N, -OFFSET_E-OFFSET_N
};
signed short knight_jump[8]=
{
    +OFFSET_E*2+OFFSET_N, +OFFSET_E*2-OFFSET_N,
    -OFFSET_E*2+OFFSET_N, -OFFSET_E*2-OFFSET_N,
    +OFFSET_N*2+OFFSET_E, +OFFSET_N*2-OFFSET_E,
    -OFFSET_N*2+OFFSET_E, -OFFSET_N*2-OFFSET_E,
};

int move_king(int fr, int offset)
{
    int temp=DIFF(offset,0);
    int file_offset=(temp&0xC)?1:0;
    int rank_offset=DIFF(temp,file_offset<<3);
    int to=fr+offset;
    if (to & 0100) return -1;
    if ((DIFF(F(fr),F(to))==file_offset )
            && (DIFF(R(fr),R(to))==rank_offset ))
        return to;
    return -1;
}
int move_knight(int fr, int offset)
{
    int to=fr+offset;
    if (to & 0100) return -1;
    if (R(fr)!=R(to) && F(fr)!=F(to)
            && DISTANCE(fr,to)==3)
    {
        return to;
    }
    return -1;
}

/*
 *      Pseudo-random generator
 */
U64 rand64_seed;
U64 rand64(void)
{
    int i;
    for (i=0; i<64; i++)
        rand64_seed ^= ((U64)(rand())) << i;
    return rand64_seed;
}

/*
 *      hash function
 */

U64 compute_hash(void)
{
    U64 hash = 0;
    int sq;

    for (sq=0; sq<64; sq++)
        if (board[sq] != EMPTY)
            hash ^= zobrist[board[sq]][sq];

    if (hash == 0) return hash;

    if (board[EP])
        hash ^= zobrist[0][32+F(board[EP])];
    hash ^= zobrist[0][board[CASTLE]];
    if (WTM) hash ^= zobrist[0][63];
    return hash;
}

/*
 *    Piece/square table && Piece Value
 */
#define PawnValueMg    198
#define PawnValueEg    258
#define KnightValueMg  817
#define KnightValueEg  846
#define BishopValueMg  836
#define BishopValueEg  857
#define RookValueMg    1270
#define RookValueEg    1281
#define QueenValueMg   2521
#define QueenValueEg   2558

int pieceValue[2][13] =
{
    {
        0,
        0, QueenValueMg, RookValueMg, BishopValueMg, KnightValueMg, PawnValueMg,
        0, QueenValueMg, RookValueMg, BishopValueMg, KnightValueMg, PawnValueMg
    },
    {
        0,
        0, QueenValueEg, RookValueEg, BishopValueEg, KnightValueEg, PawnValueEg,
        0, QueenValueEg, RookValueEg, BishopValueEg, KnightValueEg, PawnValueEg,
    }

};
int tapered_piece_value(pc)
{
    return tapered(pieceValue[0][pc], pieceValue[1][pc]);
}

void piece_square_table_init(void)
{
    const int CTF[] = {  1,  2,  3,  4,  4,  3,  2,  1};
    const int CTR[] = {  0,  1,  2,  3,  4,  3,  2,  1};

    const int KMF[] = {204,212,175,137,137,175,212,204};
    const int KMR[] = {291,205,136,137, 94, 70, 48, 31};
    const int KEF[] = {132,187,224,227,227,224,187,132};
    const int KER[] = {112,159,191,204,227,197,161,111};

    const int QMF[] = { -1,  8, 10,  7,  7, 10,  8, -1};
    const int QMR[] = { -1,  8,  9,  7,  7, 10,  6,  0};
    const int QEF[] = {-29, -5,  9, 17, 17,  9, -5,-29};
    const int QER[] = {-29, -4,  5, 17, 23,  2, -7,-30};

    const int RMF[] = {-22, -6, -1,  2,  2, -1, -6,-22};
    const int RMR[] = { -9,  0,  2,  2,  1,  2, 12, -5};

    const int BMF[] = {-21, 18, 11,  0,  0, 11, 18,-21};
    const int BMR[] = {-44, -9,  1,  0, -1, -8,-12,-39};
    const int BEF[] = {-36,-13,-15,  7,  7,-15,-13,-36};
    const int BER[] = {-28, -5,  8,  7,  3,  1, -4,-27};

    const int NMF[] = {-25, 18, 43, 47, 47, 43, 18,-25};
    const int NMR[] = {-73,-10,  9, 47, 50, 71, 14,-29};
    const int NEF[] = {-41,-25,  2, 38, 38,  2,-25,-41};
    const int NER[] = {-14,  9, 28, 38, 41, 27, 13,-13};

    const int PMF[] = {-25,-14, 20, 35, 35, 20,-14,-25};
    const int PMR[] = {  0,  3, 24, 35, 21, -2, -4,  0};
    const int PEF[] = {  1,  3, -8, -3, -3, -8,  3,  1};
    const int PER[] = {  0, -2,  4, -3, -6,  4, 18,  0};

    int sq,f,r,MG,EG;
    for (sq=0; sq<64; sq++)
    {
        f=F(sq);
        r=R(sq);
        /* contral table */

        piece_square[0][0][sq]=CTF[f]+CTR[r]+1*(r>RANK_4);

        /* kings */

        MG=KMF[f]+KMR[r];
        EG=KEF[f]+KER[r];
        piece_square[0][WHITE_KING][sq]=MG;
        piece_square[0][BLACK_KING][FLIP(sq)]=-MG;
        piece_square[1][WHITE_KING][sq]=EG;
        piece_square[1][BLACK_KING][FLIP(sq)]=-EG;

        /* queens rook */
        MG=QMF[f]+QMR[r];
        EG=QEF[f]+QER[r];
        piece_square[0][WHITE_QUEEN][sq]=MG;
        piece_square[0][BLACK_QUEEN][FLIP(sq)]=-MG;
        piece_square[1][WHITE_QUEEN][sq]=EG;
        piece_square[1][BLACK_QUEEN][FLIP(sq)]=-EG;

        MG=RMF[f]+RMR[r];
        EG = 0;
        piece_square[0][WHITE_ROOK][sq]=MG;
        piece_square[0][BLACK_ROOK][FLIP(sq)]=-MG;
        piece_square[1][WHITE_ROOK][sq]=EG;
        piece_square[1][BLACK_ROOK][FLIP(sq)]=-EG;

        /**< bishop   knight */
        MG=BMF[f]+BMR[r];
        EG=BEF[f]+BER[r];
        piece_square[0][WHITE_KNIGHT][sq]=MG;
        piece_square[0][BLACK_KNIGHT][FLIP(sq)]=-MG;
        piece_square[0][WHITE_BISHOP][sq]=MG;
        piece_square[0][BLACK_BISHOP][FLIP(sq)]=-MG;

        MG=NMF[f]+NMR[r];
        EG=NEF[f]+NER[r];
        piece_square[1][WHITE_KNIGHT][sq]=EG;
        piece_square[1][BLACK_KNIGHT][FLIP(sq)]=-EG;
        piece_square[1][WHITE_BISHOP][sq]=EG;
        piece_square[1][BLACK_BISHOP][FLIP(sq)]=-MG;


        /* pawn */
        MG=PMF[f]+PMR[r];
        EG=PEF[f]+PER[r];
        piece_square[0][WHITE_PAWN][sq]=MG;
        piece_square[0][BLACK_PAWN][FLIP(sq)]=-MG;
        piece_square[1][WHITE_PAWN][sq]=EG;
        piece_square[1][BLACK_PAWN][FLIP(sq)]=-EG;
    }
}
int piece_square_value(int pc, int sq)
{
    return tapered(piece_square[0][pc][sq], piece_square[1][pc][sq]);
}

/*
 *      fill side structs
 */

void fss_slide(int sq, struct side *s)
{
    int i;
    int to;
    int pc = board[sq];
    int start = pc == WHITE_BISHOP || pc == BLACK_BISHOP ? 4 : 0;
    int end = pc == WHITE_ROOK || pc == BLACK_ROOK ? 3 : 7;

    for (i=start; i<=end; i++)
    {
        to=move_king(sq,king_step[i]);
        while (to!=-1)
        {
            s->attack[to] += 1;
            if (board[to] != EMPTY) break;
            to=move_king(to,king_step[i]);
        }
    }
}

void fill_side_struct(void)
{
    int sq, to, pc;
    int i;

    memset(piece_counter, 0, sizeof piece_counter);
    memset(&white, 0, sizeof white);
    memset(&black, 0, sizeof black);
    memset(black.pawn,7,sizeof(black.pawn));
    self = WTM ? &white : &black;
    opp = WTM ? &black : &white;


    for (sq=0; sq<64; sq++)
    {
        pc = board[sq];
        if (pc == EMPTY) continue;

        piece_counter[pc]++;

        switch (pc)
        {
        case WHITE_KING:
            white.king=sq;
            for (i=0; i<8; i++)
            {
                to=move_king(sq,king_step[i]);
                if (to!=-1)
                    white.attack[to] += 1;
            }
            break;

        case BLACK_KING:
            black.king=sq;
            for (i=0; i<8; i++)
            {
                to=move_king(sq,king_step[i]);
                if (to!=-1)
                    black.attack[to] += 1;
            }
            break;

        case WHITE_QUEEN:
            fss_slide(sq, &white);
            break;

        case BLACK_QUEEN:
            fss_slide(sq, &black);
            break;

        case WHITE_ROOK:
            fss_slide(sq, &white);
            break;

        case BLACK_ROOK:
            fss_slide(sq, &black);
            break;

        case WHITE_BISHOP:
            fss_slide(sq, &white);
            break;

        case BLACK_BISHOP:
            fss_slide(sq, &black);
            break;

        case WHITE_KNIGHT:
            for (i=0; i<8; i++)
            {
                to=move_knight(sq,knight_jump[i]);
                if (to!=-1)
                {
                    white.attack[to] += 1;
                }
            }
            break;

        case BLACK_KNIGHT:
            for (i=0; i<8; i++)
            {
                to=move_knight(sq,knight_jump[i]);
                if (to!=-1)
                {
                    black.attack[to] += 1;
                }
            }
            break;

        case WHITE_PAWN:
            if (white.pawn[1+F(sq)]<R(sq))
                white.pawn[1+F(sq)]=R(sq);
            if (F(sq) != FILE_H)
            {
                white.attack[sq + OFFSET_N + OFFSET_E] += 1;
            }
            if (F(sq) != FILE_A)
            {
                white.attack[sq + OFFSET_N - OFFSET_E] += 1;
            }
            break;

        case BLACK_PAWN:
            if (black.pawn[1+F(sq)]>R(sq))
                black.pawn[1+F(sq)]=R(sq);
            if (F(sq) != FILE_H)
            {
                black.attack[sq - OFFSET_N + OFFSET_E] += 1;
            }
            if (F(sq) != FILE_A)
            {
                black.attack[sq - OFFSET_N - OFFSET_E] += 1;
            }
            break;
        }
    }

    for (i=1; i<=12; i++)
    {
        if (PIECE_COLOR(i))
            white.mat+=tapered_piece_value(i)*piece_counter[i];
        else
            black.mat+=tapered_piece_value(i)*piece_counter[i];
    }


    if (white.mat == black.mat)
    {
        stronger = (ply&1)==(init_ply&1) ? self : opp;
        weaker =   (stronger == self) ? opp  : self;
    }
    else
    {
        stronger = white.mat>black.mat ? &white : &black;
        weaker =   (stronger == self) ? opp  : self;
    }

    hash_arr[ply]=compute_hash();
}

/*
 *      move and undo move
 */
void del_piece(short sq)
{
    *undo_sp++ = board[sq];
    board[sq] = 0;
    *undo_sp++ = sq;
}
void add_piece(short sq,short piece)
{
    if (board[sq])
        del_piece(sq);
    else
    {
        *undo_sp++ = 0;
        *undo_sp++ = sq;
    }
    board[sq] = piece;
}
void undo_move(void)
{
    int sq;
    while(undo_sp>undo_stack)
    {
        sq = *--undo_sp;
        if (sq < 0)
        {
            board[last_src]=board[last_dest];
            board[last_dest]=*--undo_sp;
            last_dest=*--undo_sp;
            last_src=*--undo_sp;
            break;
        }
        board[sq] = *--undo_sp;
    }
    ply--;
    fill_side_struct();
}
void do_move(int move)
{
    int to = TO(move);
    int fr = FR(move);
    int pc = board[fr];
    int sq;
    int castle_flag = 0;

    if (move==0) return;

    ply++;
    *undo_sp++ = last_src;
    *undo_sp++ = last_dest;
    *undo_sp++ = board[to];
    *undo_sp++ = -1;
    last_src=fr;
    last_dest=to;

    board[fr]=EMPTY;
    board[to]=pc;

    /* Clear en-passant flag */
    if (board[EP]) del_piece(EP);

    if (move == SPECIAL) return;

    if (board[to]!=EMPTY ||
            board[fr]==WHITE_PAWN || board[fr]==BLACK_PAWN)
        add_piece(RULE50,ply);

    if (move & SPECIAL)                     /* Special moves */
    {
        switch (R(fr))
        {
        case RANK_8:                    /* Black CASTLe */
            castle_flag = board[CASTLE] & (CASTLE_BLACK_LONG|CASTLE_BLACK_LONG);
            add_piece(CASTLE,castle_flag|CASTLE_WHITW_DONE);
            if (to == G8)
            {
                del_piece(H8);
                add_piece(F8,BLACK_ROOK);
            }
            else
            {
                del_piece(A8);
                add_piece(D8,BLACK_ROOK);
            }
            break;

        case RANK_7:
            if (board[to] == BLACK_PAWN)   /* Set en-passant flag */
            {
                add_piece(EP,to + OFFSET_N);
            }
            else                    /* White promotes */
            {
                add_piece(to,WHITE_QUEEN + (move>>13));
            }
            break;

        case RANK_5:                    /* White captures en-passant */
        case RANK_4:                    /* Black captures en-passant */
            sq = SQ(F(to),R(fr));
            del_piece(sq);
            break;

        case RANK_2:
            if (board[to] == WHITE_PAWN)   /* Set en-passant flag */
            {
                add_piece(EP,to - OFFSET_N);
            }
            else                    /* Black promotes */
            {
                add_piece(to,BLACK_QUEEN + (move>>13));
            }
            break;

        case RANK_1:                    /* White CASTLING */
            castle_flag = board[CASTLE] & (CASTLE_WHITE_LONG|CASTLE_WHITE_SHORT);
            add_piece(CASTLE,castle_flag|CASTLE_BLACK_DONE);
            if (to == G1)
            {
                del_piece(H1);
                add_piece(F1,WHITE_ROOK);
            }
            else
            {
                del_piece(A1);
                add_piece(D1,WHITE_ROOK);
            }
            break;

        default:
            break;
        }
    }

    if ((!castle_flag) &&
            (board[CASTLE] & (castle_sq[fr] | castle_sq[to])))
    {
        pc=board[CASTLE] &~(castle_sq[fr] | castle_sq[to]);
        add_piece(CASTLE,pc);
    }

    fill_side_struct();
}

/*
 *    Pseudo-legal moves generator
 */
int cmp_move_asc(const void *pa,const void *pb)
{
    const struct move *a=pa,*b=pb;
    if (a->prescore < b->prescore) return -1;
    if (a->prescore > b->prescore) return 1;
    return (int)(b->move - a->move);
}
int cmp_move_desc(const void *pa,const void *pb)
{
    const struct move *a=pa,*b=pb;
    if (a->prescore < b->prescore) return 1;
    if (a->prescore > b->prescore) return -1;
    return (int)(b->move - a->move);
}

int test_illegal(int move)
{
    int r;
    do_move(move);
    r = (self->attack[opp->king] != 0);
    undo_move();
    return r;
}
void push_move(int fr, int to)
{
    unsigned short prescore = PRESCORE_BASE;
    int move = MOVE(fr, to);
    int c=0,d=0;

    if (board[to] != EMPTY)
        c += tapered_piece_value(board[to]);
    if (board[EP] == to)
    {
        int sq=SQ(F(to),R(fr));
        c += tapered_piece_value(board[sq]);
    }

    if (WTM)
    {
        if (black.attack[to] != 0)
            d = tapered_piece_value(board[fr]);
    }
    else
    {
        if (white.attack[to] != 0)
            d = tapered_piece_value(board[fr]);
    }

    if ((board[fr] == WHITE_KING || board[fr] == BLACK_KING)
            && d != 0 )
    {
        prescore = PRESCORE_CAPTURES;
    }
    if (c!=0 || d!=0)
    {
        prescore += (c-d);
        prescore += (c>=d)?PRESCORE_CAPTURES+PRESCORE_CAPTURES:0;
    }
    else
        prescore += piece_square_value(0,to)-piece_square_value(0,fr);

    move_sp->move = move;
    move_sp->prescore = prescore | histroy[move];
    move_sp++;
}

void push_special_move(int fr, int to, BYTE is_good)
{
    int move= MOVE(fr, to);
    move_sp->prescore = PRESCORE_BASE;
    move_sp->prescore += is_good?PRESCORE_CAPTURES-1:histroy[move];
    move_sp->move = move | SPECIAL;
    move_sp++;
}

void push_pawn_promotion(int fr, int to, int prom_piece)
{
    int move= MOVE(fr, to);
    move_sp->prescore = PRESCORE_BASE+PRESCORE_CAPTURES-prom_piece-1;
    move_sp->move = move | SPECIAL | (prom_piece<<13);
    move_sp++;
}

void push_pawn_move(int fr, int to)
{
    if ((R(to) == RANK_8) || (R(to) == RANK_1))
    {
        push_pawn_promotion(fr, to, 0);      /* queen promotion */
        push_pawn_promotion(fr, to, 1);      /* rook promotion */
        push_pawn_promotion(fr, to, 2);      /* bishop promotion */
        push_pawn_promotion(fr, to, 3);      /* knight promotion */
    }
    else
    {
        push_move(fr, to);
    }
}
void gen_all_slides(int fr)
{
    int i;
    int to;
    int pc = board[fr];
    int start = pc == WHITE_BISHOP || pc == BLACK_BISHOP ? 4 : 0;
    int end = pc == WHITE_ROOK || pc == BLACK_ROOK ? 3 : 7;

    for (i=start; i<=end; i++)
    {
        to=move_king(fr,king_step[i]);
        while (to!=-1)
        {
            if (board[to] != EMPTY)
            {
                if (PIECE_COLOR(board[to]) != WTM)
                {
                    push_move(fr, to);
                }
                break;
            }
            push_move(fr, to);
            to=move_king(to,king_step[i]);
        }
    }
}
void gen_all(void)
{
    int fr,to,p/* piece */;
    int i;

    for (fr=0; fr<64; fr++)
    {
        p=board[fr];
        if (p==EMPTY || PIECE_COLOR(p) != WTM) continue;

        switch(p)
        {
        case WHITE_KING:
            for (i=0; i<8; i++)
            {
                to=move_king(fr,king_step[i]);
                if (to!=-1)
                {
                    if (black.attack[to]>0) continue;
                    if (board[to] != EMPTY && PIECE_COLOR(board[to]) == WTM)
                        continue;
                    push_move(fr, to);
                }
            }
            break;


        case BLACK_KING:
            for (i=0; i<8; i++)
            {
                to=move_king(fr,king_step[i]);
                if (to!=-1)
                {
                    if (white.attack[to]>0) continue;
                    if (board[to] != EMPTY && PIECE_COLOR(board[to]) == WTM)
                        continue;
                    push_move(fr, to);
                }
            }
            break;

        case WHITE_QUEEN:
        case BLACK_QUEEN:

        case WHITE_ROOK:
        case BLACK_ROOK:


        case WHITE_BISHOP:
        case BLACK_BISHOP:
            gen_all_slides(fr);
            break;

        case WHITE_KNIGHT:
        case BLACK_KNIGHT:
            for (i=0; i<8; i++)
            {
                to=move_knight(fr,knight_jump[i]);
                if (to!=-1)
                {
                    if (board[to] != EMPTY &&
                            PIECE_COLOR(board[to]) == WTM) continue;
                    push_move(fr, to);
                }
            }
            break;

        case WHITE_PAWN:
            if (F(fr) != FILE_H)
            {
                to = fr + OFFSET_N + OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 0)
                {
                    push_pawn_move(fr, to);
                }
            }
            if (F(fr) != FILE_A)
            {
                to = fr + OFFSET_N - OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 0)
                {
                    push_pawn_move(fr, to);
                }
            }
            to = fr + OFFSET_N;
            if (board[to] != EMPTY) break;
            push_pawn_move(fr, to);
            if (R(fr) == RANK_2)
            {
                to += OFFSET_N;
                if (board[to] == EMPTY)
                {
                    if (F(to)!=FILE_A && board[to-OFFSET_E]==BLACK_PAWN)
                    {
                        push_special_move(fr, to, 0);
                    }
                    else if (F(to)!=FILE_H && board[to+OFFSET_E]==BLACK_PAWN)
                    {
                        push_special_move(fr, to, 0);
                    }
                    else
                        push_move(fr, to);
                }
            }
            break;

        case BLACK_PAWN:
            if (F(fr) != FILE_H)
            {
                to = fr - OFFSET_N + OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 1)
                {
                    push_pawn_move(fr, to);
                }
            }
            if (F(fr) != FILE_A)
            {
                to = fr - OFFSET_N - OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 1)
                {
                    push_pawn_move(fr, to);
                }

            }
            to = fr - OFFSET_N;
            if (board[to] != EMPTY) break;
            push_pawn_move(fr, to);
            if (R(fr) == RANK_7)
            {
                to -= OFFSET_N;
                if (board[to] == EMPTY)
                {
                    if (F(to)!=FILE_A && board[to-OFFSET_E]==WHITE_PAWN)
                    {
                        push_special_move(fr, to, 0);
                    }
                    else if (F(to)!=FILE_H && board[to+OFFSET_E]==WHITE_PAWN)
                    {
                        push_special_move(fr, to, 0);
                    }
                    else
                        push_move(fr, to);
                }
            }
            break;
        }
    }

    if (board[CASTLE] && !opp->attack[self->king] &&
            (board[E1] == WHITE_KING || board[E8] == BLACK_KING))
    {
        if (WTM && (board[CASTLE] & CASTLE_WHITE_SHORT) &&
                board[H1] == WHITE_ROOK &&
                !board[F1] && !board[G1] &&
                !black.attack[F1] && !black.attack[G1])
        {
            push_special_move(E1, G1, 1);
        }
        if (WTM && (board[CASTLE] & CASTLE_WHITE_LONG) &&
                board[A1] == WHITE_ROOK &&
                !board[D1] && !board[C1] && !board[B1] &&
                !black.attack[D1] && !black.attack[C1])
        {
            push_special_move(E1, C1, 1);
        }
        if (!WTM && (board[CASTLE] & CASTLE_BLACK_SHORT) &&
                board[H8] == BLACK_ROOK &&
                !board[F8] && !board[G8] &&
                !white.attack[F8] && !white.attack[G8])
        {
            push_special_move(E8, G8, 1);
        }
        if (!WTM && (board[CASTLE] & CASTLE_BLACK_LONG) &&
                board[A8] == BLACK_ROOK &&
                !board[D8] && !board[C8] && !board[B8] &&
                !white.attack[D8] && !white.attack[C8])
        {
            push_special_move(E8, C8, 1);
        }
    }
    if (board[EP])
    {
        if (WTM)
        {
            if (F(board[EP]) != FILE_A && board[board[EP]-OFFSET_E-OFFSET_N] == WHITE_PAWN)
            {
                push_special_move(board[EP]-OFFSET_E-OFFSET_N, board[EP], 1);
            }
            if (F(board[EP]) != FILE_H && board[board[EP]+OFFSET_E-OFFSET_N] == WHITE_PAWN)
            {
                push_special_move(board[EP]+OFFSET_E-OFFSET_N, board[EP], 1);
            }
        }
        else
        {
            if (F(board[EP]) != FILE_A && board[board[EP]-OFFSET_E+OFFSET_N] == BLACK_PAWN)
            {
                push_special_move(board[EP]-OFFSET_E+OFFSET_N, board[EP], 1);
            }
            if (F(board[EP]) != FILE_H && board[board[EP]+OFFSET_E+OFFSET_N] == BLACK_PAWN)
            {
                push_special_move(board[EP]+OFFSET_E+OFFSET_N, board[EP], 1);
            }
        }
    }
}

void gen_caps_slides(int fr)
{
    int i;
    int to;
    int pc = board[fr];
    int start = pc == WHITE_BISHOP || pc == BLACK_BISHOP ? 4 : 0;
    int end = pc == WHITE_ROOK || pc == BLACK_ROOK ? 3 : 7;

    for (i=start; i<=end; i++)
    {
        to=move_king(fr,king_step[i]);
        while (to!=-1)
        {
            if (board[to] != EMPTY)
            {
                if (PIECE_COLOR(board[to]) != WTM)
                {
                    push_move(fr, to);
                }
                break;
            }
            to=move_king(to,king_step[i]);
        }
    }
}
void gen_caps(void)
{
    int fr,to,p/* piece */;
    int i;

    for (fr=0; fr<64; fr++)
    {
        p=board[fr];
        if (p==EMPTY || PIECE_COLOR(p) != WTM) continue;

        switch(p)
        {
        case WHITE_KING:
            for (i=0; i<8; i++)
            {
                to=move_king(fr,king_step[i]);
                if (to!=-1)
                {
                    if (black.attack[to]>0) continue;
                    if (board[to] != EMPTY && PIECE_COLOR(board[to]) != WTM)
                        push_move(fr, to);
                }
            }
            break;

        case BLACK_KING:
            for (i=0; i<8; i++)
            {
                to=move_king(fr,king_step[i]);
                if (to!=-1)
                {
                    if (white.attack[to]>0) continue;
                    if (board[to] != EMPTY && PIECE_COLOR(board[to]) != WTM)
                        push_move(fr, to);
                }
            }
            break;

        case WHITE_BISHOP:
        case BLACK_BISHOP:

        case WHITE_QUEEN:
        case BLACK_QUEEN:

        case WHITE_ROOK:
        case BLACK_ROOK:
            gen_caps_slides(fr);
            break;

        case WHITE_KNIGHT:
        case BLACK_KNIGHT:
            for (i=0; i<8; i++)
            {
                to=move_knight(fr,knight_jump[i]);
                if (to!=-1)
                {
                    if (board[to] != EMPTY &&
                            PIECE_COLOR(board[to]) != WTM)
                        push_move(fr, to);
                }
            }
            break;

        case WHITE_PAWN:
            if (F(fr) != FILE_H)
            {
                to = fr + OFFSET_N + OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 0)
                {
                    push_pawn_move(fr, to);
                }
            }
            if (F(fr) != FILE_A)
            {
                to = fr + OFFSET_N - OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 0)
                {
                    push_pawn_move(fr, to);
                }
            }

            break;

        case BLACK_PAWN:
            if (F(fr) != FILE_H)
            {
                to = fr - OFFSET_N + OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 1)
                {
                    push_pawn_move(fr, to);
                }
            }
            if (F(fr) != FILE_A)
            {
                to = fr - OFFSET_N - OFFSET_E;
                if (board[to] != EMPTY && PIECE_COLOR(board[to]) == 1)
                {
                    push_pawn_move(fr, to);
                }
            }
            break;
        }
    }
    if (board[EP])
    {
        if (WTM)
        {
            if (F(board[EP]) != FILE_A && board[board[EP]-OFFSET_E-OFFSET_N] == WHITE_PAWN)
            {
                push_special_move(board[EP]-OFFSET_E-OFFSET_N, board[EP], 1);
            }
            if (F(board[EP]) != FILE_H && board[board[EP]+OFFSET_E-OFFSET_N] == WHITE_PAWN)
            {
                push_special_move(board[EP]+OFFSET_E-OFFSET_N, board[EP], 1);
            }
        }
        else
        {
            if (F(board[EP]) != FILE_A && board[board[EP]-OFFSET_E+OFFSET_N] == BLACK_PAWN)
            {
                push_special_move(board[EP]-OFFSET_E+OFFSET_N, board[EP], 1);
            }
            if (F(board[EP]) != FILE_H && board[board[EP]+OFFSET_E+OFFSET_N] == BLACK_PAWN)
            {
                push_special_move(board[EP]+OFFSET_E+OFFSET_N, board[EP], 1);
            }
        }
    }
}


/*
 *      I/O functions
 */

void print_square(int square)
{
    putchar(FILE2CHAR(F(square)));
    putchar(RANK2CHAR(R(square)));
}
void print_move(int move)
{
    int fr, to;
    struct move *m;

    fr = FR(move);
    to = TO(move);

    if ((move==(MOVE(E1,C1)|SPECIAL)) || (move==(MOVE(E8,C8)|SPECIAL)))
    {
        printf("0-0-0");
    }
    else if ((move==(MOVE(E1,G1)|SPECIAL)) || (move==(MOVE(E8,G8)|SPECIAL)))
    {
        printf("0-0");
    }
    else if ((board[fr]==WHITE_PAWN) || (board[fr] == BLACK_PAWN))
    {
        /* capture sign */
        if (F(fr) != F(to))
        {
            printf("%cx", FILE2CHAR(F(fr)));
        }
        print_square(to);

        /* promote to piece */
        if (R(to)==RANK_1 || R(to)==RANK_8)
        {
            putchar("QRBN"[move>>13]);
        }
    }
    else
    {
        int r=0,f=0;

        putchar(toupper(PIECE2CHAR(board[fr])));

        m = move_sp;
        gen_all();
        while (move_sp > m)
        {
            move_sp--;
            if (board[fr] != board[FR(move_sp->move)]
                    || move == move_sp->move
                    || to != TO(move_sp->move))
                continue;

            f |= (F(fr) == F(FR(move_sp->move))) ;
            r |= (R(fr) == R(FR(move_sp->move))) ;
        }
        if (f) putchar(FILE2CHAR(F(fr)));
        if (r && f!= r) putchar(RANK2CHAR(R(fr)));

        if (board[to] != EMPTY) putchar('x');

        print_square(to);
    }

    do_move(move);
    if (opp->attack[self->king])
    {
        int sign = '#';
        m = move_sp;
        gen_all();
        while (move_sp > m)
        {
            move_sp--;
            if (test_illegal(move_sp->move))
                continue;;
            sign = '+';
            move_sp = m;        /* break */
        }
        putchar(sign);
    }
    undo_move();
}
void print_move_long(int move)
{
    print_square(FR(move));
    print_square(TO(move));
    if (move & SPECIAL)
    {
        if ((board[FR(move)] == WHITE_PAWN && R(TO(move)) == RANK_8) ||
                (board[FR(move)] == BLACK_PAWN && R(TO(move)) == RANK_1))
        {
            putchar("QRBN"[move >> 13]);
        }
    }
}

BYTE bd_help_msg=1;
void print_board_wcs(void)
{
    int t,i;
    for (i=1; i<=6; i++)
    {
        t=captures[i]+captures[i+6];
        if (t>0)
        {
            if (captures[i]>0)
                printf(" %cx%d",PIECE2CHAR(i),captures[i]);
            else
                printf("    ");
        }
    }
}
void print_board_bcs(void)
{
    int t,i;
    for (i=1; i<=6; i++)
    {
        t=captures[i]+captures[i+6];
        if (t>0)
        {
            if (captures[i+6]>0)
                printf(" %cx%d",PIECE2CHAR(i),captures[i+6]);
            else
                printf("    ");
        }
    }
}

void print_board(void)
{
    const char aux_str[5] = " :*#";
    const char col_str[3] = "bw";
    char pc_ch,col_ch,aux_ch;

    int i, j,sq;
    short disp[64];
    short aux[64];


    for (i = 0; i < 64; i++)
    {
        disp[i] = board[i];
        aux[i]=0;
        if ((F(i)&1)==(R(i)&1))
        {
            if(disp[i]==0) disp[i]=13;
            aux[i]=1;
        }
    }
    if (last_src!=last_dest)
    {
        disp[last_src]=aux[last_src]?15:14;
        aux[last_dest]=2;
    }

    for (i = 7; i >= 0; i--)
    {
        printf("\t\t    +---+---+---+---+---+---+---+---+\n");

        printf("\t\t %2d |",i+1);
        for (j = 0; j < 8; j++)
        {
            sq=SQ(j,i);
            aux_ch = aux_str[(i^j)&1];
            if (board[sq] == EMPTY)
            {
                col_ch = aux_ch;
                pc_ch = last_src == sq ? aux_str[2] : aux_ch;
            }
            else
            {
                col_ch = col_str[PIECE_COLOR(board[sq])];
                pc_ch = toupper(PIECE2CHAR(board[sq]));
            }
            aux_ch = last_dest == sq ? aux_str[3] : aux_ch;

            printf("%c%c%c|", col_ch, pc_ch, aux_ch);
        }

        if (caps[0]>0 && i == 7)
            print_board_bcs();
        if (caps[1]>0 && i == 0)
            print_board_wcs();

        printf("\n");
    }

    printf("\t\t    +-A-+-B-+-C-+-D-+-E-+-F-+-G-+-H-+");

    printf("\n");

}

int readline(char *line, int size,FILE* fp)
{
    char ch;
    int i=0;

    do
    {
        ch = fgetc(fp);
        if (ch == EOF)
        {
            return -1;
        }
        if (i < size-1)
        {
            line[i] = ch;
            i++;
        }
    }
    while(ch!='\n');
    line[i] = '\0';
    return i;
}

/*
 *    evaluator
 */
int eval_white_king_pawn(void)
{
    int r=0,i,f=F(white.king);
    for (i=f; i<=f+2; i++)
    {
        if (white.pawn[f]==0)
            r+=10;
        else if(white.pawn[f]>R(white.king))
            r+=white.pawn[f]-R(white.king);
    }
    return r;

}
int eval_black_king_pawn(void)
{
    int r=0,i,f=F(black.king);
    for (i=f; i<=f+2; i++)
    {
        if (black.pawn[f]==7)
            r+=10;
        else if(black.pawn[f]<R(white.king))
            r+=R(white.king)-black.pawn[f];
    }
    return r;
}
int eval(short int mode)
/*
mode=0:return -INF to -INF+2 when drawn.CLI functions calls.
mode=1:return DRAWN_VALUE when drawn.   search functions calls.
*/
{
    int sq,pc,f;
    int score;
    int w,b,t;
    int drawn=(mode==0)?-INF:DRAWN_VALUE;

    score=(white.mat-black.mat);

    /* first:compute piece values*/
    w=white.king;
    b=black.king;
    for (sq=0; sq<64; sq++)
    {
        f=F(sq)+1;
        pc=board[sq];

        {
            int t = white.attack[sq] - black.attack[sq];
            if (t == 0)
                /**< do nothig */;
            else if (t>0)
                score += piece_square_value(0, sq)*t;
            else
                score -= piece_square_value(0, FLIP(sq))*t;
        }
        if (pc==EMPTY)
        {
            continue;
        }

        score += piece_square_value(pc, sq);

        switch(pc)
        {
        case WHITE_KING:
            t = eval_white_king_pawn()*8;
            score -= tapered(t, 0);
            break;

        case BLACK_KING:
            t = eval_white_king_pawn()*8;
            score += tapered(t, 0);
            break;

        case WHITE_ROOK:
            if (white.pawn[f]==0)
            {
                score+=tapered(19, 10);
                if (black.pawn[f]==7)
                    score+=tapered(43, 21);
            }
            else if (((F(w) < FILE_E) == (F(sq) < F(w)))
                     && (R(w) == R(sq) || R(w) == RANK_1))
                score -= (tapered(92, 0) - tapered(piece_square[0][WHITE_ROOK][sq], 0))
                         * (1 + ((board[CASTLE] & (CASTLE_WHITE_SHORT|CASTLE_WHITE_LONG))==0));

            if (black.pawn[f]!=7)
                score+=tapered(10, 28);

            break;
        case BLACK_ROOK:
            if (black.pawn[f]==7)
            {
                score-=tapered(19, 10);
                if (white.pawn[f]==0)
                    score-=tapered(43, 21);
            }
            else if (((F(b) < FILE_E) == (F(sq) < F(b)))
                     && (R(w) == R(sq) || R(w) == RANK_8))
                score += (tapered(92, 0) - tapered(piece_square[0][BLACK_ROOK][sq], 0))
                         * (1 + (board[CASTLE] & (CASTLE_BLACK_LONG|CASTLE_BLACK_SHORT))==0);

            if (white.pawn[f]!=0)
                score-=tapered(10, 28);
            break;

        case WHITE_QUEEN:
            break;
        case BLACK_QUEEN:
            break;


        case WHITE_KNIGHT:
            if (    R(sq) < RANK_5
                    && board[sq + OFFSET_N] == WHITE_PAWN)
                score += tapered(16, 0);
            break;
        case BLACK_KNIGHT:
            if (    R(sq) >= RANK_5
                    && board[sq - OFFSET_N] == WHITE_PAWN)
                score -= tapered(16, 0);
            break;

        case WHITE_BISHOP:
            if (    R(sq) < RANK_5
                    && board[sq + OFFSET_N] == WHITE_PAWN)
                score += tapered(16, 0);
            break;
        case BLACK_BISHOP:
            if (    R(sq) >= RANK_5
                    && board[sq - OFFSET_N] == WHITE_PAWN)
                score -= tapered(16, 0);
            break;

        case WHITE_PAWN:
            /* penish for doubled */
            if (white.pawn[f] > R(sq))
                score -= 7;

            /* penish for isolated */
            if ((white.pawn[f - 1] == 0) &&
                    (white.pawn[f + 1] == 0))
                score -= 10;

            /* penish for backwards */
            else if ((white.pawn[f - 1] < R(sq)) &&
                     (white.pawn[f + 1] < R(sq)))
                score -= 5;
            break;

            /* bonus for passed */
            if ((black.pawn[f - 1] == 7) &&
                    (black.pawn[f] == 7) &&
                    (black.pawn[f + 1] == 7))
                score += R(sq);

        case BLACK_PAWN:
            /* penish for doubled */
            if (black.pawn[f] < R(sq))
                score += 7;

            /* penish for isolated */
            if ((black.pawn[f - 1] == 7) &&
                    (black.pawn[f + 1] == 7))
                score += 10;

            /* penish for backwards */
            else if ((black.pawn[f - 1] > R(sq)) &&
                     (black.pawn[f + 1] > R(sq)))
                score += 5;

            /* bonus for passed */
            if ((white.pawn[f - 1] == 0) &&
                    (white.pawn[f] == 0) &&
                    (white.pawn[f + 1] == 0))
                score -= 7-R(sq);
            break;

        default:
            break;
        }
    }


    /* ending? */
    if (ending() && piece_counter[WHITE_PAWN]+piece_counter[BLACK_PAWN]==0)
    {
        if (white.mat+black.mat < tapered_piece_value(WHITE_ROOK))
            return drawn;

    }

    if (mode==0)
    {
        int i,count=0;
        /* draw by 50th moves rules */
        if (board[RULE50]>100)
        {
            return -INF+2;
        }
        /* draw by postion repeats 3 times */
        for (i=ply-4; i>=board[RULE50]; i-=2)
        {
            if (hash_arr[i] == hash_arr[ply]) count++;
            if (count>=2) return -INF+1;
        }
    }

    return WTM ? score : -score;
}

/*
 *   move parser
 */
int parse_move(char *line,int *offset)
{
    int                     move[50]= {0}, matches=0;
    int                     n = 0;
    struct move             *m;
    char                    *piece = NULL;
    char                    *fr_file = NULL;
    char                    *fr_rank = NULL;
    char                    *to_file = NULL;
    char                    *to_rank = NULL;
    char                    *prom_piece = NULL;
    int                     caps_flag = 0;

    while(isspace(line[n])) n++;
    if (!strncmp(line+n, "o-o-o", 5)
            ||  !strncmp(line+n, "O-O-O", 5)
            ||  !strncmp(line+n, "0-0-0", 5))
    {
        piece = "K";
        fr_file = "e";
        to_file = "c";
        n+=5;
    }
    else if (!strncmp(line+n, "o-o", 3)
             ||  !strncmp(line+n, "O-O", 3)
             ||  !strncmp(line+n, "0-0", 3))
    {
        piece = "K";
        fr_file = "e";
        to_file = "g";
        n+=3;
    }
    else
    {
        if (line[n]=='K' || line[n]=='Q'
                || line[n]=='R' || line[n]=='B'
                || line[n]=='N' || line[n]=='P')
        {
            piece = line+n;
            n++;
        }

        if (line[n]>='a' && line[n]<='h')
        {
            to_file = line+n;
            n++;
        }
        if (line[n]>='1' && line[n]<='8')
        {
            to_rank = line+n;
            n++;
        }

        if (line[n] == '-' || line[n] == 'x')
        {
            if (line[n] == 'x') caps_flag = 1;
            if (line[n] == '-') caps_flag = 2;
            fr_file = to_file;
            fr_rank = to_rank;
            to_file = NULL;
            to_rank = NULL;
            n++;
        }

        if (line[n]>='a' && line[n]<='h')
        {
            if (fr_file == NULL)
            {
                fr_file = to_file;
                fr_rank = to_rank;
            }
            to_file = line+n;
            to_rank = NULL;
            n++;
        }
        if (line[n]>='1' && line[n]<='8')
        {
            to_rank = line+n;
            n++;
        }

        while (line[n] == '=')
        {
            n++;
        }
        if (line[n]=='Q' || line[n]=='N'
                || line[n]=='R' || line[n]=='B'
                || line[n]=='q' || line[n]=='n'
                || line[n]=='r' || line[n]=='b')
        {
            if (piece==NULL)
            {
                prom_piece = line+n;
                n++;
            }
            else if (*piece=='P')
            {
                prom_piece = line+n;
                n++;
            }
        }
    }
    if (!strncmp(line+n, "e.p.", 4))
    {
        n+=4;
    }
    while (line[n] == '+' || line[n] == '#' || line[n] == '!'
            || line[n] == '?' || line[n] == ',')
    {
        n++;
    }

    *offset=n;

    if (!piece && !fr_file && !fr_rank
            && !to_file && !to_rank && !prom_piece)
        return -1;

    m = move_sp;
    gen_all();
    while (move_sp > m)
    {
        int fr, to;

        move_sp--;

        fr = FR(move_sp->move);
        to = TO(move_sp->move);
        if (fr == to) continue;

        if ((piece && *piece != toupper(PIECE2CHAR(board[fr])))
                || (to_file && *to_file != FILE2CHAR(F(to)))
                || (to_rank && *to_rank != RANK2CHAR(R(to)))
                || (fr_file && *fr_file != FILE2CHAR(F(fr)))
                || (fr_rank && *fr_rank != RANK2CHAR(R(fr)))
                || (prom_piece &&
                    (toupper(*prom_piece) != "QRBN"[(move_sp->move)>>13]))
                || (caps_flag == 1 && board[to] == EMPTY)
                || (caps_flag == 2 && board[to] != EMPTY))
            continue;

        if (test_illegal(move_sp->move))
            continue;

        if (matches)
        {
            int old_pawn, new_pawn;

            if (piece!=NULL)
            {
                move[matches] = move_sp->move;
                matches++;
                continue;
            }

            old_pawn = (board[FR(move[matches-1])]==WHITE_PAWN) ||
                       (board[FR(move[matches-1])]==BLACK_PAWN);
            new_pawn = (board[fr]==WHITE_PAWN) ||
                       (board[fr]==BLACK_PAWN);

            if (new_pawn)
            {
                if (!old_pawn)
                {
                    move[0] = move_sp->move;
                    matches = 1;
                }
                else
                {
                    move[matches] = move_sp->move;
                    matches++;
                }
            }
            else if (!old_pawn)
            {
                move[matches] = move_sp->move;
                matches++;
            }
        }
        else
        {
            move[0] = move_sp->move;
            matches = 1;
        }
    }
    if (matches == 1)
        return move[0];
    else if (matches > 1)
    {
        print_board();
        printf("有%d个着法匹配:\n",matches);
        for (n=0; n<matches; n++)
        {
            print_move(move[n]);
            printf("\t");
        }
        printf("\n");
        return 0;
    }
    return -1;
}
/*
 *      Opening book                                              |
 */

int cmp_book(const void *ap, const void *bp)
{
    const struct mem *a = ap;
    const struct mem *b = bp;
    if (a->hash < b->hash) return -1;
    if (a->hash > b->hash) return 1;
    return (int)(b->move - a->move);
}
void compress_core(void)
{
    long b = 0, c = 0;

    qsort(core, book_size, sizeof(struct mem), cmp_book);
    while (b<book_size)
    {
        core[c] = core[b];
        b++;
        while (b<book_size && !cmp_book(&core[c], &core[b]))
        {
            core[c].score+=core[b].score;
            b++;
        }
        c++;
    }
    book_size = c;
}
void load_book(char *filename)
{
    FILE                    *fp;
    char                    line[128], *s;
    int                     num, move;

    book_size = 0;

    fp = fopen(filename, "r");
    if (!fp)
    {
        printf("错误：缺少开局库文件（%s）!\n", filename);
        return;
    }
    while (readline(line, sizeof(line), fp) >= 0)
    {
        s = line;
        if (*s=='#') continue;
        for (;;)
        {
            move = parse_move(s, &num);
            if (move<=0)
            {
                if (move==0)
                    printf("开局库相关行:%s\n",line);
                break;
            }

            s += num;
            if (book_size < CORE)
            {
                core[book_size].hash = compute_hash();
                core[book_size].move = move;
                core[book_size].score = 1;
                core[book_size].flag = TTFLAG_BOOK;
                book_size++;
                if (book_size >= CORE) compress_core();
            }
            do_move(move);
        }
        while (ply>0)
            undo_move();
    }
    fclose(fp);
    compress_core();
}

int search_book(void)
{
    unsigned int move = 0,sum=0;
    long l = 0, r, m;
    U64 hash = hash_arr[ply];

    if (!book_size) return 0;
    r = book_size;
    while (r - l > 1)
    {
        m = (l + r) / 2;
        if (hash < core[m].hash)
        {
            r=m;
        }
        else
        {
            l=m;
        }
    }
    while (core[l].hash == hash)
    {
        printf(" (%d)",core[l].score);
        print_move(core[l].move);
        sum+=core[l].score;
        r=core[l].hash | rand64();
        m=r%sum;
        if (m<core[l].score)
            move=core[l].move;
        if (!l--) break;
    }
    printf("\n");

    return move;
}

/*
 *      position
 */
void clear(void)
{
    book_size=0;
    memset(core,0,sizeof(core));
    memset(histroy,0,sizeof(histroy));
    memset(killer_move,0,sizeof(killer_move));
}
void reset(void)
{
    move_sp = move_stack;
    undo_sp = undo_stack;
    fill_side_struct();
    memset(histroy,0,sizeof(histroy));
    memset(killer_move,0,sizeof(killer_move));
    rand64_seed=time(NULL);

    if (opening())
    {
        load_book(OPENING_BOOK_FILENAME);
    }
    else
    {
        book_size=0;
        memset(core,0,sizeof(core));
    }
}

void setup_board(char *fen)
{
    int file=FILE_A, rank=RANK_8;
    int rule,round;
    int t=0,n=0;
    char order,cstr[5]= {'\0'},ep[3]= {'\0'},bstr[64+8]= {'\0'};

    while (isspace(*fen)) fen++;
    if(sscanf(fen,"%s %c %s %s %d %d",bstr,&order,cstr,ep,&rule,&round)!=6)
    {
        puts("ERROR: Bad FEN-string format!");
        return;
    }

    memset(board, 0, sizeof board);
    while (rank>RANK_1 || file<=FILE_H)
    {
        int piece = EMPTY;

        switch (bstr[n])
        {
        case 'K':
            t+=3;
            if (t>3||t==1)
            {
                puts("ERROR: Bad FEN-string format!");
                memset(board, 0, sizeof board);
                hash_arr[ply]=0;
                return;
            }
            piece = WHITE_KING;
            break;
        case 'Q':
            piece = WHITE_QUEEN;
            break;
        case 'R':
            piece = WHITE_ROOK;
            break;
        case 'B':
            piece = WHITE_BISHOP;
            break;
        case 'N':
            piece = WHITE_KNIGHT;
            break;
        case 'P':
            if (rank==RANK_1||rank==RANK_8)
            {
                puts("错误：局面非法(兵不能出现在第1及第8横线)!");
                puts("提示：棋盘已清空!请重新用fen命令设置，或者用new命令开始新游戏。");
                memset(board, 0, sizeof board);
                hash_arr[ply]=0;
                return;
            }
            piece = WHITE_PAWN;
            break;
        case 'k':
            t--;
            if (t<-1||t==1)
            {
                puts("错误：局面非法(棋子数量不正确,各方必须有且只能有1个王)!");
                puts("提示：棋盘已清空!请重新用fen命令设置，或者用new命令开始新游戏。");
                memset(board, 0, sizeof board);
                hash_arr[ply]=0;
                return;
            }
            piece = BLACK_KING;
            break;
        case 'r':
            piece = BLACK_ROOK;
            break;
        case 'q':
            piece = BLACK_QUEEN;
            break;
        case 'b':
            piece = BLACK_BISHOP;
            break;
        case 'n':
            piece = BLACK_KNIGHT;
            break;
        case 'p':
            if (rank==RANK_1||rank==RANK_8)
            {
                puts("ERROR: Bad FEN-string format!");
                memset(board, 0, sizeof board);
                hash_arr[ply]=0;
                return;
            }
            piece = BLACK_PAWN;
            break;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
            file += (bstr[n] - '0');
            break;

        case '/':
            rank -= 1;
            file = FILE_A;
            n++;
            continue;

        case ' ':
        case '\0':
            rank = -1;
            file = 8;
            break;

        default:
            puts("ERROR: Bad FEN-string format!");
            memset(board, 0, sizeof board);
            hash_arr[ply]=0;
            return;
        }

        if (piece != EMPTY)
        {
            board[SQ(file,rank)] = piece;
            file++;
        }
        n++;
    }
    if (t!=2)
    {
        print_board();
        puts("ERROR: Bad FEN-string format!");
        memset(board, 0, sizeof board);
        hash_arr[ply]=0;
        return;
    }

    switch (order)
    {
    case 'w':
        ply = 0;
        break;
    case 'b':
        ply = 1;
        break;
    default:
        puts("ERROR: Bad FEN-string format!");
        memset(board, 0, sizeof board);
        hash_arr[ply]=0;
        return;
    }

    fill_side_struct();

    if (self->attack[opp->king])
    {
        ply++;
        move_sp=move_stack;
        gen_all();
        while (move_sp>move_stack)
        {
            move_sp--;
            if(!test_illegal(move_sp->move))
            {
                print_board();
                puts("ERROR: Illegal position (left king in check)!");
                memset(board, 0, sizeof board);
                move_sp=move_stack;
                hash_arr[ply]=0;
                return;
            }
        }
        move_sp=move_stack;
        ply--;
    }

    n=0;
    if (cstr[0] != '-')
    {
        while (cstr[n])
        {
            switch (cstr[n])
            {
            case 'K':
                board[CASTLE] |= CASTLE_WHITE_SHORT;
                break;
            case 'Q':
                board[CASTLE] |= CASTLE_WHITE_LONG;
                break;
            case 'k':
                board[CASTLE] |= CASTLE_BLACK_SHORT;
                break;
            case 'q':
                board[CASTLE] |= CASTLE_BLACK_LONG;
                break;
            default:
                puts("ERROR: Bad FEN-string format!");
                memset(board, 0, sizeof board);
                hash_arr[ply]=0;
                return;
            }
            n++;
        }
    }

    if (ep[0] != '-')
    {
        if  (ep[0]>='a' && ep[0]<='h'
                && (ep[1]=='3' || ep[1]=='6'))
            board[EP] = SQ(CHAR2FILE(ep[0]),CHAR2RANK(ep[1]));
        else
        {
            puts("ERROR: Bad FEN-string format!");
            memset(board, 0, sizeof board);
            hash_arr[ply]=0;
            return;
        }
    }

    round= MAX(round,1);
    rule=ply-rule;
    ply += (round-1)<<1;
    init_ply = ply;
    board[RULE50] =MAX(rule,0);
    last_src = last_dest = -1;

    reset();
}

/*
 *    Search
 */
unsigned long nodes,start_ply;
clock_t timer,start_time,time_log;
BYTE stop_search;
#define WIN 32000
#define MATE (WIN-100)
void init_timer(void)
{
    time_log = start_time=clock();
    timer=start_time+time_limit - CLOCKS_PER_SEC;
    stop_search=0;
}
int search_contral(int depth)
{
    if (depth > 99)
        stop_search=1;
    else if (time_limit != INF)
    {
        clock_t t = clock();
        if (t > timer)
            stop_search=2;
        else
            stop_search=0;
    }
    else
        stop_search=(depth<=maxdepth)?0:1;
    return stop_search;
}

int search_full(int depth, int alpha, int beta)
{
    int best_move = 0;
    int score;
    struct move *moves = move_sp;
    int in_check = opp->attack[self->king];
    int best_score = -INF;
    struct mem tt;
    int ttflag = TTFLAG_ALPHA;
    int i;
    int new_depth;
    int move;
    int ttable_move = 0;
    int PV_move = 0;

    nodes++;

    /* draw by 50 moves rules */
    if (ply-board[RULE50] > 100) return DRAWN_VALUE;

    /* draw by postion repetetion */
    for (i=ply-4; i>=board[RULE50]; i-=2)
        if (hash_arr[i] == hash_arr[ply]) return DRAWN_VALUE;

    /* check transposition table */
    tt = core[(hash_arr[ply]&(CORE-1))];
    if (tt.hash == hash_arr[ply] && tt.flag != TTFLAG_BOOK)
    {
        if (tt.depth >= depth)
        {
            if (tt.flag == TTFLAG_PV)
                return tt.score;
            else if ((tt.flag == TTFLAG_ALPHA) && (tt.score <= alpha))
                return alpha;
            else if ((tt.flag == TTFLAG_BETA) && (tt.score >= beta))
                return beta;
        }
        ttable_move = tt.move;
    }

    if (ply >= MAX_PLY || search_contral(0) == 2)
        return eval(1);

    pv_rear[ply]=pv_rear[ply+1]=0;
    if (ply <= pv_rear[start_ply])
        PV_move = pv[start_ply][ply];

    histroy[PV_move&07777]              |= PRESCORE_PV_MOVE;
    histroy[ttable_move&07777]          |= PRESCORE_TTABLE_BEST;
    histroy[killer_move[ply][0]&07777]  |= PRESCORE_KILLER_MOVE;
    histroy[killer_move[ply][1]&07777]  |= PRESCORE_KILLER_MOVE;

    if (!in_check && depth <= 0)
        gen_caps();
    else
        gen_all();

    qsort(moves, move_sp-moves, sizeof(*moves), cmp_move_asc);

    histroy[PV_move&07777]              &= (PRESCORE_CAPTURES-1);
    histroy[ttable_move&07777]          &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][0]&07777]  &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][1]&07777]  &= (PRESCORE_CAPTURES-1);

    while (move_sp > moves)
    {
        move_sp--;
        move = move_sp->move;

        do_move(move);
        if (self->attack[opp->king])
        {
            undo_move();
            continue;
        }

        new_depth = in_check ? depth : depth-1;
        if (depth > 0 && ttflag == TTFLAG_PV)
        {
            score = -search_full(new_depth, -alpha-1, -alpha);
            if (score>alpha && score<beta)
                score = -search_full(new_depth, -beta, -alpha);
        }
        else
            score = -search_full(new_depth, -beta, -alpha);
        if (score >= MATE) score--;
        if (score <= -MATE) score++;

        undo_move();

        if (score > best_score ||
                (move == best_move &&
                 pv_rear[ply] < pv_rear[ply+1]))
        {
            best_score = score;
            best_move = move;

            pv[ply][ply] = move;
            if (pv_rear[ply+1]>ply)
            {
                for (i=ply+1; i<=pv_rear[ply+1]; i++)
                    pv[ply][i]=pv[ply+1][i];
                pv_rear[ply]=pv_rear[ply+1];
            }
            else
                pv_rear[ply]=ply;

            if (score >= beta)
            {
                if (move != killer_move[ply][0])
                {
                    killer_move[ply][1]=killer_move[ply][0];
                    killer_move[ply][0]=move;
                }
                ttflag = TTFLAG_BETA;
                move_sp = moves;
                break;
            }
            else if (score > alpha)
            {
                ttflag = TTFLAG_PV;
                alpha = score;
            }
        }

        if (clock() - time_log > CLOCKS_PER_SEC)
        {
            time_log = clock();
            printf("info refutation" );
            for (i=ply; i<=pv_rear[ply]; i++)
            {
                printf(" ");
                print_move_long(pv[ply][i]);
            }
            printf("\n");
        }
    }

    /* checkmate or stalemate */
    if (best_score == -INF)
    {
        if (in_check)
            best_score = -WIN;
        else if (depth <= 0)
            best_score = eval(1);
        else
            best_score = DRAWN_VALUE;
    }

    {
        tt.score = best_score;
        tt.hash = hash_arr[ply];
        tt.depth = depth;
        tt.flag = ttflag;
        tt.move = best_move;
        core[(hash_arr[ply]&(CORE-1))] = tt;

        if (best_move & 07777)
        {
            if (best_score >= MATE || best_score <= -MATE)
                histroy[best_move&07777] = PRESCORE_CAPTURES-1;
            else
            {
                histroy[best_move&07777] += depth*depth;
                while (histroy[best_move&07777] >= PRESCORE_CAPTURES)
                    for (i=0; i<=07777; i++)
                        histroy[i] >>= 1;
            }
        }
    }

    return best_score;
}

int search_main(void)
{
    int depth = 1;
    int score;
    int move;
    int best_score = -INF;;
    int alpha=-INF, beta=+INF, delta;
    unsigned long n;
    struct move *m;
    int in_check = 0;
    struct mem tt;
    int new_depth;
    int ttable_move = 0;
    int i;
    int bestmovechange,low_fail;
    clock_t ply_start_time,time_log,used;

    init_timer();
    in_check = opp->attack[self->king];

    hash_arr[ply]=compute_hash();
    if (hash_arr[ply] == 0)
    {
        memset(computer,0,sizeof(computer));
        return 0;
    }

    move = search_book();
    if (move) return move;
    if (book_size>0) book_size=0;

    nodes = 0;
    start_ply=ply;
    move_sp=move_stack;
    memset(pv_rear,0,sizeof(pv_rear));

    tt = core[(hash_arr[ply]&(CORE-1))];
    if (tt.hash == hash_arr[ply] && tt.flag != TTFLAG_BOOK)
        ttable_move = tt.move;

    histroy[ttable_move&07777]         |= PRESCORE_TTABLE_BEST;
    histroy[killer_move[ply][0]&07777] |= PRESCORE_KILLER_MOVE;
    histroy[killer_move[ply][1]&07777] |= PRESCORE_KILLER_MOVE;

    gen_all();

    histroy[ttable_move&07777]         &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][0]&07777] &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][1]&07777] &= (PRESCORE_CAPTURES-1);

    qsort(move_stack, move_sp-move_stack, sizeof(struct move), cmp_move_desc);

    while (search_contral(depth)==0)
    {
        time_log = ply_start_time = clock();
        bestmovechange = low_fail = 0;
        m = move_stack;
        printf("info depth %d\n",depth);


        while (m < move_sp)
        {
            n=nodes;

            do_move(m->move);
            if (self->attack[opp->king] != 0)
            {
                undo_move();
                *m = *--move_sp;
                continue;
            }

            if (clock() - time_log > CLOCKS_PER_SEC)
            {
                time_log = clock();
                printf("info depth %d "
                       "currmovenumber %d currmove ",
                       depth,
                       m-move_stack+1);
                print_move_long(m->move);
                printf("\n");
            }

            nodes++;
            new_depth = in_check ? depth : depth-1;

            score = -search_full(new_depth, -beta, -alpha);

            undo_move();
            m->prescore = nodes-n;

            if (score > best_score ||
                    (m->move == move &&
                     pv_rear[ply] < pv_rear[ply+1]))
            {
                struct move temp;

                if (score > best_score)
                    bestmovechange = 1;

                best_score = score;
                move = m->move;

                temp = *move_stack;
                *move_stack = *m;
                *m = temp;

                pv[ply][ply] = move;
                if (pv_rear[ply+1]>ply)
                {
                    int i;
                    for (i=ply+1; i<=pv_rear[ply+1]; i++)
                        pv[ply][i]=pv[ply+1][i];
                    pv_rear[ply]=pv_rear[ply+1];
                }
                else
                    pv_rear[ply]=ply;
            }

            m++;
        }

        delta = 18 + 5 * depth;
        if (best_score >= beta)
        {
            alpha = (alpha + beta) / 2;
            beta = best_score + delta;
        }
        else if (best_score < alpha)
        {
            low_fail = 1;
            alpha = best_score - delta;
            beta = (alpha + beta) / 2;
        }
        else
        {
            alpha = best_score - delta;
            beta =best_score + delta;
        }

        {
            time_log = clock();
            used = MAX(1,(time_log-ply_start_time));

            printf("info depth %d time %lu nodes %lu nps %lu score ",
                   depth,
                   used,
                   nodes,
                   nodes / MAX(1, used / CLOCKS_PER_SEC) );
            if (abs(best_score) <= MATE)
                printf ("cp %d", best_score * 100 / PawnValueEg);
            else
                printf ("mate %d", (best_score > 0 ? WIN - best_score + 1 : -MATE - best_score) / 2);
            if (best_score >= beta)
                printf(" lowerbound");
            else if(best_score <= alpha)
                printf(" upperbound");

            printf(" pv");
            for (i=ply; i<=pv_rear[ply]; i++)
            {
                printf(" ");
                print_move_long(pv[ply][i]);
            }
            printf("\n");
        }

        if (best_score+depth >= WIN) break;
        if (best_score-depth <= -WIN) break;
        if (move_sp-move_stack <= 1) break;

        qsort(move_stack+1, move_sp-move_stack-1, sizeof(*m), cmp_move_asc);

        if (time_limit !=INF)
        {
            used = time_log - ply_start_time ;
            if (time_log + used * (1+bestmovechange+low_fail) >= timer)
                break;
        }

        depth++;
    }

    {
        time_log = clock();
        used = MAX(1,(time_log-start_time));

        printf("info time %lu nodes %lu nps %lu score ",
               used,
               nodes,
               nodes / MAX(1, used / CLOCKS_PER_SEC));
        if (abs(best_score) <= MATE)
            printf ("cp %d", best_score * 100 / PawnValueEg);
        else
            printf ("mate %d", (best_score > 0 ? WIN - best_score + 1 : -MATE - best_score) / 2);
        if (best_score >= beta)
            printf(" lowerbound");
        else if(best_score <= alpha)
            printf(" upperbound");

        printf(" pv");
        for (i=ply; i<=pv_rear[ply]; i++)
        {
            printf(" ");
            print_move_long(pv[ply][i]);
        }
        printf("\n");
    }

    move_sp = move_stack;
    return move;
}

/*
 *    CLI commands
 */
void cmd_board(char *dummy )
{
    UNUSED(dummy)
    print_board();
}
void cmd_book(char *dummy)
{
    UNUSED(dummy)
    search_book();
}
void cmd_new(char *dummy)
{
    UNUSED(dummy)
    setup_board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    print_board();
    computer[0] = 1;
    computer[1] = 0;
    computer[2] = 0;
}
void cmd_set_depth(char *s)
{
    if (1==sscanf(s, "%*s%d", &maxdepth))
    {
        time_limit=INF;
        maxdepth = MAX(1, maxdepth);
    }
    if (time_limit != INF)
        printf("Max search time limit to %ld second(s).\n", (time_limit/CLOCKS_PER_SEC));
    else
        printf("Max search death limit to %d plys.\n", maxdepth);
}
void cmd_set_time_limit(char *s)
{
    if (1==sscanf(s, "%*s%ld", &time_limit))
    {
        maxdepth=MAX_PLY;
        time_limit *= CLOCKS_PER_SEC;
        time_limit = MAX(1, time_limit);
    }
    if (time_limit != INF)
        printf("Max search time limit to %ld second(s).\n", (time_limit/CLOCKS_PER_SEC));
    else
        printf("Max search death limit to %d plys.\n", maxdepth);
}
void cmd_both(char *dummy)
{
    UNUSED(dummy)
    memset(computer, 1, sizeof(computer));
    computer[2] = 0;
}
void cmd_white(char *dummy)
{
    UNUSED(dummy)
    memset(computer, 0, sizeof(computer));
    computer[1] = 1;
}
void cmd_black(char *dummy)
{
    UNUSED(dummy)
    memset(computer, 0, sizeof(computer));
    computer[0] = 1;
}
void cmd_go(char *dummy)
{
    UNUSED(dummy)
    memset(computer, 0, sizeof(computer));
    computer[2] = 1;
}
void cmd_human(char *dummy)
{
    UNUSED(dummy)
    memset(computer,0,sizeof(computer));
}
void cmd_list(char *dummy)
{
    UNUSED(dummy)
    struct move *m = move_sp;
    int n = 0,ttable_move=0;
    struct mem tt = core[(hash_arr[ply]&(CORE-1))];

    puts("Moves:");

    if (tt.hash == hash_arr[ply] && tt.flag != TTFLAG_BOOK)
        ttable_move = tt.move;

    histroy[ttable_move&07777]         |= PRESCORE_TTABLE_BEST;
    histroy[killer_move[ply][0]&07777] |= PRESCORE_KILLER_MOVE;
    histroy[killer_move[ply][1]&07777] |= PRESCORE_KILLER_MOVE;

    gen_all();
    qsort(m, move_sp - m, sizeof(*m), cmp_move_asc);

    histroy[ttable_move&07777]         &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][0]&07777] &= (PRESCORE_CAPTURES-1);
    histroy[killer_move[ply][1]&07777] &= (PRESCORE_CAPTURES-1);

    while (move_sp > m)
    {
        --move_sp;
        if (test_illegal(move_sp->move)) continue;
        print_move(move_sp->move);
        putchar('\t');
        n++;
    }
    printf("\ntotal %d move(s).\n",n);
}
void cmd_print_fen(char *dummy)
{
    UNUSED(dummy)
    int sq=A8,piece,count;

    while (sq<64&&sq>=0)
    {
        piece=board[sq];
        if (piece==EMPTY)
        {
            count=0;
            while(F(sq)!=FILE_H&&board[sq]==EMPTY)
            {
                count++;
                sq+=OFFSET_E;
            }
            printf("%d",count+((F(sq)==FILE_H)?1:0));
            if (sq<64) piece=board[sq];
        }
        if (sq<64&&piece!=EMPTY)
            putchar(PIECE2CHAR(piece));

        if (sq==H1)
        {
            break;
        }
        else if (F(sq)==FILE_H)
        {
            putchar('/');
            sq=SQ(FILE_A,(R(sq)-1));
        }
        else
            sq+=OFFSET_E;
    }

    printf(" %s ",WTM?"w":"b");

    if (board[CASTLE])
        printf("%s%s%s%s ",
               board[CASTLE] & CASTLE_WHITE_SHORT ? "K" : "",
               board[CASTLE] & CASTLE_WHITE_LONG ? "Q" : "",
               board[CASTLE] & CASTLE_BLACK_SHORT ? "k" : "",
               board[CASTLE] & CASTLE_BLACK_LONG ? "q" : "");
    else
        printf("- ");

    if (board[EP])
        print_square(board[EP]);
    else
        printf("-");
    printf(" %ld %ld\n",ply-board[RULE50],ply/2+1);

}
void cmd_undo(char *dummy)
{
    UNUSED(dummy)
    if (undo_sp > undo_stack)
    {
        undo_move();
        if (computer[0]+computer[1])
            undo_move();
        print_board();
    }
    else
    {
        puts("ERROR：Can not undo now!\n");
    }
}
void cmd_eval(char *dummy)
{
    UNUSED(dummy)
    int i,in_check;
    struct move *m=move_sp;
    int r;

    if (ply >= MAX_PLY)
    {
        printf("GAME OVER：Drawn（by so many plys）!\n");
        memset(computer,0,sizeof(computer));
    }

    r=eval(0);
    printf("%s\'s score:%+d,white mat.:%d,black mat.:%d."
           ,WTM?"White":"Black",(r<=-MATE||r>=MATE)?DRAWN_VALUE:r*100/PawnValueEg,
           white.mat,black.mat);
    if (hash_arr[ply]==0)
        printf("board is empty\n");
    else
        printf("current phase:%s.\n",
               ending()?"endgame":(opening()?"opening":"mid-game"));



    gen_all();
    in_check = opp->attack[self->king];

    while (m<move_sp)
    {
        move_sp--;
        if (!test_illegal(move_sp->move))
        {
            if (r<=-MATE || r>=MATE)
            {
                switch (r)
                {
                case -INF:
                    printf("GAME OVER: Drawn(Neither can mate)!\n");
                    break;
                case -INF+1:
                {
                    int count=1;
                    printf("GAME OVER: Drawn(same position at %ld",ply/2+!WTM);
                    for (i=ply-4; i>=board[RULE50]; i-=2)
                    {
                        if (hash_arr[i] == hash_arr[ply])
                        {
                            count++;
                            printf(",%d",i/2+!WTM);
                        }

                    }
                    printf(")!\n");
                    break;
                }
                case -INF+2:
                    printf("GAMEOVER: Drawn(By 50-moves rule)!\n");
                    break;
                default:
                    printf("GAMEOVER: Drawn!\n");
                    break;
                }
                memset(computer,0,sizeof(computer));
                move_sp=m;
                return;
            }
            if (in_check)
                printf("Note: %s in check\n",WTM?"White":"Black");
            move_sp=m;
            return;
        }
    }
    if (in_check)
        printf("GAME OVER: %s Won!\n",!WTM?"White":"Black");
    else
        printf("GAME OVER: Drawn(Stalemate)!\n");
    memset(computer,0,sizeof(computer));
}
void cmd_fen(char *s)
{
    while (isalpha(*s)) s++;

    setup_board(s);
    print_board();

    memset(computer,0,sizeof(computer));
    cmd_eval(NULL);

}
void cmd_default(char *s)
{
    int move,dummy;

    move = parse_move(s,&dummy);
    if (move>0)
    {
        if (ply >= MAX_PLY)
        {
            printf("GAME OVER: Drawn(so many moves)!\n");
            memset(computer,0,sizeof(computer));
            return;
        }

        printf("%ld. %s", 1+ply/2,(WTM) ? "" : "... ");
        print_move(move);
        printf("\n");
        do_move(move);
        print_board();
        cmd_eval(NULL);
    }
    else if (move<0)
    {
        printf("ERROR: Unknow command!\n");
    }
}
void cmd_test(char *s)
{
    UNUSED(s)
    int book=book_size,move;
    book_size = 0;
    move=search_main();
    if (move)
    {
        printf("Result: ");
        print_move(move);
        printf("\n");
    }
    if (book!=0) load_book(OPENING_BOOK_FILENAME);
}
void cmd_reopen(char *dummy)
{
    UNUSED(dummy)
    load_book(OPENING_BOOK_FILENAME);
}
void cmd_clear(char *dummy)
{
    UNUSED(dummy)
    clear();
}

struct command
{
    char *cmd;
    void (*handler)(char*);
    char *help;
} commands[];

#define CMD_QUIT_STR "quit"
void cmd_help(char *dummy)
{
    UNUSED(dummy)
    struct command *c;
    bd_help_msg=1;
    puts("Commands(format: <command> [argument]。):");
    c = commands;
    while (c->cmd != NULL)
    {
        printf("%-8s %s\n", c->cmd ? c->cmd : "", c->help);
        c++;
    }
    printf("%-8s %s\n", CMD_QUIT_STR, "Quit program");
}

struct command commands[] =
{
    { "show",       cmd_board,      "show chessboard."                           },
    { "ls",         cmd_list,       "list moves."                   },
    { "new",        cmd_new,        "new games."                           },
    { "undo",       cmd_undo,       "undo move."                               },
    { "human",      cmd_human,      "human vs human."                           },
    { "white",      cmd_white,      "computer vs human."                 }, /* 5 */
    { "black",      cmd_black,      "human vs computer."                 },
    {"both",        cmd_both,       "computer vs computer.(note:demo mode)."},
    { "go",         cmd_go,         "computer go next move.(will set mode to human vs human after that.)"                     },
    {"sd",          cmd_set_depth,  "set/show search depth."  },
    {"st",       cmd_set_time_limit, "set/show  search time limit."},
    { "test",       cmd_test,       "search test."                           }, /* 10 */
    { "eval",       cmd_eval,       "show eval info."                           },
    { "book",       cmd_book,       "find in opening book."         },
    { "fen",        cmd_fen,        "set position by given FEN"},
    { "getfen",     cmd_print_fen,  "show FEN for current position."       },
    { "reload",     cmd_reopen,     "reload opening book"                     }, /* 15 */
    { "clear",      cmd_clear,      "clear trans table"             },
    { "help",       cmd_help,       "show this list"                     },
    { NULL,         cmd_default,    NULL                                   },
};

void handle_command(char* name,char* line)
{
    int i;
    for (i=0; commands[i].cmd != NULL; i++)
    {
        if (stricmp(commands[i].cmd, name)==0)
        {
            break;
        }
    }
    commands[i].handler(line);
}

/*
 *    Main and initialization functions
 */
char *startup_message=
    "HJCHESS " VERSION " (C) HE Jun (aka SkyWolf,Jeremy) 2012-2016\n";

void response_move(void)
{
    int move;
    if (ply >= MAX_PLY)
    {
        printf("GAME OVER：DRAWN（PLY >= MAX_PLY）!\n");
        memset(computer,0,sizeof(computer));
    }

    while (computer[2] || computer[WTM])
    {
        move = search_main();
        if (move && ply < MAX_PLY)
        {
            printf("%ld. %s", 1+ply/2,(WTM) ? "" : "... ");
            print_move(move);
            printf("\n");

            do_move(move);
        }
        print_board();
        cmd_eval(NULL);

        if (computer[2])
            memset(computer,0,sizeof(computer));
    }
}

void init(void)
{
    int i,j;
    puts(startup_message);
    piece_square_table_init();

    rand64_seed = time(NULL);

    for (i=0; i<13 ; i++)
        for (j=0; j<64; j++)
            zobrist[i][j] = rand64();

    castle_sq[A1] = CASTLE_WHITE_LONG;
    castle_sq[E1] = CASTLE_WHITE_SHORT | CASTLE_WHITE_LONG;
    castle_sq[H1] = CASTLE_WHITE_SHORT;
    castle_sq[A8] = CASTLE_BLACK_LONG;
    castle_sq[E8] = CASTLE_BLACK_SHORT | CASTLE_BLACK_LONG;
    castle_sq[H8] = CASTLE_BLACK_SHORT;

    cmd_new(NULL);
}

int main(void)
{
    char line[80];
    char name[80];

    init();

    while(1)   /* main loop */
    {
        printf("\n%s%lu>",WTM?"white":"black",(ply/2)+1);
        if (readline(line, sizeof(line),stdin) < 0)
            break;
        if (1 != sscanf(line, "%s", name)) continue;
        if (stricmp(CMD_QUIT_STR, name)==0)
            break;
        handle_command(name,line);

        response_move();
    }
    return 0;
}
