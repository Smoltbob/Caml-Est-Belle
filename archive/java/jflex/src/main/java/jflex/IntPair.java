/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * JFlex 1.6                                                               *
 * Copyright (C) 1998-2014  Gerwin Klein <lsf@jflex.de>                    *
 * All rights reserved.                                                    *
 *                                                                         *
 * License: BSD                                                            *
 *                                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package jflex;


/**
 * Simple pair of integers.
 *
 * Used in NFA to represent a partial NFA by its start and end state.
 *
 * @author Gerwin Klein
 * @version JFlex 1.6, $Revision: 815 $, $Date: 2014-05-19 03:07:52 +1000 (Mon, 19 May 2014) $
 */
final class IntPair {

  int start;
  int end;
  
  IntPair(int start, int end) {
    this.start = start;
    this.end = end;
  }

  public int hashCode() {
    return end + (start << 8);
  }  
  
  public boolean equals(Object o) {
    if ( o instanceof IntPair ) {
      IntPair p = (IntPair) o;
      return start == p.start && end == p.end;
    }
    return false;
  }
  
  public String toString() {
    return "("+start+","+end+")";
  }
} 
