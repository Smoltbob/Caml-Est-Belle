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
 * This Exception is used in class CharClasses.
 *
 * @author Gerwin Klein
 * @version JFlex 1.6, $Revision: 815 $, $Date: 2014-05-19 03:07:52 +1000 (Mon, 19 May 2014) $
 */
public class CharClassException extends RuntimeException {

  /**
	 * 
	 */
	private static final long serialVersionUID = 7199804506062103569L;


  /**
   * Creates a new CharClassException without message
   */
  public CharClassException() {
  }


  /**
   * Creates a new CharClassException with the specified message
   *
   * @param message   the error description presented to the user.
   */
  public CharClassException(String message) {
    super(message);
  }

}
