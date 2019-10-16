#include "Programs/TTT.hpp"
#include <cstdio>
#include <algorithm>
#include <cmath>
#include<vector>
#include<iostream>

using namespace std;


double TTT::EvalState(const State s) const
{
	int row_count = 0, row_count2 = 0, col_count = 0, col_count2 = 0, diag_count = 0, diag_count2 = 0, anti_diag_count = 0, anti_diag_count2 = 0, rId;
	Value a = FIRST, b = SECOND, c = EMPTY;
	double rScore = 0.0, cScore = 0.0, adScore = 0.0, dScore = 0.0, score = 0.0;
	
	int rowcol = m_nrRows * m_nrCols;
	
	int count_empty_r = 0;
	int col = m_nrCols;
	for (int r = 0; r < rowcol; r = r + m_nrCols)
	{
		int count_row = 0;
		rId = RowFromId(r);
		for (int k = r; k - r + m_nrToWin < m_nrCols + 1; k++)
		{
			for (int c = k - r; c < m_nrToWin + k - r; c++)
			{

				if (GetValueState(s, rId, c) == a)
				{
					row_count++;
				}
				if (GetValueState(s, rId, c) == b)
				{
					row_count2++;
				}
			}
			if (((row_count + row_count2) == m_nrToWin) && (row_count>0) && (row_count2>0))
			{
				rScore = rScore + 0;
			}
			else
			{
				if (row_count!= 0)
				{
					rScore += pow(2, row_count - 1);
				}
				if (row_count2 != 0)
				{
					rScore -= pow(2, row_count2 - 1);
				}

			}
			row_count= 0;
			row_count2 = 0;
		}
	}

	//Evaluates Coloumn
	State ts = NewState();
	
	for (int r = 0; r<m_nrRows; r++)
	{
		for (int c = 0; c < m_nrCols; c++)
		{
			SetValueState(ts, c, r, GetValueState(s, r, c));
		}
	}
	for (int r = 0; r < rowcol; r = r + m_nrRows)
	{
		int count_row = 0;
		rId = RowFromId(r);
		for (int k = r; k - r + m_nrToWin < m_nrRows + 1; k++)
		{
			if (((col_count + col_count2) == m_nrToWin) && (col_count > 0) && (col_count2 > 0))
			{
				for (int c = k - r; c < m_nrToWin + k - r; c++)
				{

					if (GetValueState(ts, rId, c) == a)
					{
						col_count++;
					}
					if (GetValueState(ts, rId, c) == b)
					{
						col_count2++;
					}
				}

			{
				cScore = cScore + 0;
			}
		}
			else
			{
				if (col_count != 0)
				{
					cScore += pow(2, col_count - 1);
				}
				if (col_count2 != 0)
				{
					cScore -= pow(2, col_count2 - 1);
				}
			}
			col_count = 0;
			col_count2 = 0;
		}

	}


	if ((m_nrToWin > m_nrCols) || (m_nrToWin > m_nrRows))
	{
		adScore = 0.0;
		dScore= 0.0;
	}
	else
	{
		//Evaluates diagnol
		for (int i = 0; i < rowcol; i++)
		{
			int r = RowFromId(i), c = ColFromId(i), count = 0;
			while ((r < m_nrRows) && (c < m_nrCols) && (count < m_nrToWin))
			{
				count++;
				if (GetValueState(s, r, c) == a)
				{
					diag_count++;
				}
				if (GetValueState(s, r, c) == b)
				{
					diag_count2++;
				}
				r++;
				c++;
			}
			if ((((diag_count + diag_count2) == m_nrToWin) && (diag_count>0) && (diag_count2>0)) || count<m_nrToWin)
			{
				dScore = dScore + 0;
			}
			else
			{
				if (diag_count != 0)
				{
					dScore += pow(2, diag_count - 1);
				}
				if (diag_count2 != 0)
				{
					dScore -= pow(2, diag_count2 - 1);
				}
			}
			diag_count = 0;
			diag_count2 = 0;
		//Evaluates antidiagnol
		for (int i = 0; i < (m_nrCols + m_nrRows - 1); i++)
		{
			int counter = 0;
			int colind = max(0, i - m_nrRows);
			int count = min(min(i, (m_nrCols - colind)), m_nrRows);
			for (int j = 0; j < count; j++)
			{
				if (count - j >= m_nrToWin) {
					counter = 1;
					for (int k = 0; k < m_nrToWin; k++) {
						counter++;
						if (GetValueState(s, min(m_nrRows, i) - k - 1, colind + k) == a)
						{
							anti_diag_count++;
						}
						if (GetValueState(s, min(m_nrRows, i) - k - 1, colind + k) == b)
						{
							anti_diag_count2++;
						}
					}
				}
				if ((((anti_diag_count + anti_diag_count2) == m_nrToWin) && (anti_diag_count>0) && (anti_diag_count2>0)) || counter<m_nrToWin)
				{
					adScore = adScore + 0;
				}
				else
				{
					if (anti_diag_count != 0)
					{
						adScore += pow(2, anti_diag_count - 1);
					}
					if (anti_diag_count2 != 0)
					{
						adScore -= pow(2, anti_diag_count2 - 1);
					}
				}
				anti_diag_count = 0;
				anti_diag_count2 = 0;

			}
		}
		
		}
	}
	score = rScore + cScore + dScore + adScore;
	return score;
}

void TTT::BestMove(State   s,
	int     depth,
	int     remMoves,
	int     lastMove,
	Value   player,
	double *bestScore,
	int    *bestMove) const
{
	

	double bestVal;
	double minmaxVal;
	int move = 0;
	int r = 0, c = 0;

	State tempState = CopyState(s);
	bestVal = NegaMax(s, depth, -MAX_VALUE, MAX_VALUE, remMoves, lastMove, player);
	if (player == Value::FIRST)
	{



		for (int i = 0; i < m_nrRows * m_nrCols; i++)
		{
			if (GetValueState(tempState, RowFromId(i), ColFromId(i)) == Value::EMPTY)
			{
				SetValueState(tempState, RowFromId(i), ColFromId(i), Value::FIRST);
				minmaxVal = NegaMax(tempState, depth - 1, -MAX_VALUE, MAX_VALUE, remMoves - 1, i, Value::SECOND);
				SetValueState(tempState, RowFromId(i), ColFromId(i), Value::EMPTY);
				
				if (minmaxVal == bestVal)
				{
					
					move = i;
					break;
					
				}
			}
		}

	}
	else if (player == Value::SECOND)
	{

		
		for (int i = 0; i < m_nrRows * m_nrCols; i++)


		{
			if (GetValueState(tempState, RowFromId(i), ColFromId(i)) == EMPTY)
			{
				SetValueState(tempState, RowFromId(i), ColFromId(i), Value::SECOND);
				minmaxVal = NegaMax(tempState, depth - 1, -MAX_VALUE, MAX_VALUE, remMoves - 1, i, Value::FIRST);
				SetValueState(tempState, RowFromId(i), ColFromId(i), Value::EMPTY);
				if (minmaxVal == bestVal)
				{
					move = i;
					break;
					
				}
			}
		}

	}
	*bestScore = bestVal;
	*bestMove = move;
	

}


double TTT::NegaMax(State   s,
	int     depth,
	double  alpha,
	double  beta,
	int     remMoves,
	int     lastMove,
	int     color) const
{
	int r = RowFromId(lastMove);
	int c = ColFromId(lastMove);
	int rowcol = m_nrRows * m_nrCols;
	double best_value;

	if (depth == 0 || remMoves == 0 || CheckWin(s, r, c) == true)
	{
		return EvalState(s);
	}
	if (color == Value::FIRST)
	{
		double val = -MAX_VALUE;
		for (int i = 0; i < m_nrRows; i++)
		{
			for (int j = 0; j < m_nrCols; j++)
			{
				if (GetValueState(s, i, j) == EMPTY)
				{
					SetValueState(s, i, j, Value::FIRST);
					val = max(val, NegaMax(s, depth - 1, alpha, beta, remMoves - 1, IdFromRowCol(i, j), Value::SECOND));
					SetValueState(s, i, j, Value::EMPTY);
					alpha = max(alpha, val);
					if (beta <= alpha)
					  break;
					
				}
			}
		}
		return val;
	}
	else if (color == Value::SECOND)
	{
		double val = MAX_VALUE;
		for (int i = 0; i < m_nrRows; i++)
		{
			for (int j = 0; j < m_nrCols; j++)
			{
				if (GetValueState(s, i, j) == EMPTY)
				{
					SetValueState(s, i, j, Value::SECOND);
					val= min(val, NegaMax(s, depth - 1, alpha, beta, remMoves - 1, IdFromRowCol(i, j), Value::FIRST));
					SetValueState(s, i, j, Value::EMPTY);
					beta = min(beta, val);
					if (beta <= alpha)
					   break;
					

				}
			}
		}
		return val;
	}

}

bool TTT::CheckWin(const State s, const int r, const int c) const
{
	const Value val = GetValueState(s, r, c);
	int   cons = 0, end;

	//check row
	cons = 0; end = std::min(m_nrCols, c + m_nrToWin);
	for (int i = std::max(0, c - m_nrToWin); i < end; ++i)
		if (GetValueState(s, r, i) != val)
			cons = 0;
		else if ((++cons) >= m_nrToWin)
			return true;

	//check column
	cons = 0; end = std::min(m_nrRows, r + m_nrToWin);
	for (int i = std::max(0, r - m_nrToWin); i < end; ++i)
		if (GetValueState(s, i, c) != val)
			cons = 0;
		else if ((++cons) >= m_nrToWin)
			return true;

	//check diagonal
	cons = 0; end = 1 + std::min(m_nrToWin - 1, std::min(m_nrRows - r - 1, m_nrCols - c - 1));
	for (int i = std::max(-m_nrToWin + 1, std::max(-r, -c)); i < end; ++i)
		if (GetValueState(s, r + i, c + i) != val)
			cons = 0;
		else if ((++cons) >= m_nrToWin)
			return true;

	//check anti-diagonal
	cons = 0; end = 1 + std::min(m_nrToWin - 1, std::min(r, m_nrCols - c - 1));
	for (int i = std::max(-m_nrToWin + 1, std::max(r - m_nrRows + 1, -c)); i < end; ++i)
	{
		if (GetValueState(s, r - i, c + i) != val)
			cons = 0;
		else if ((++cons) >= m_nrToWin)
			return true;
	}


	return false;


}





