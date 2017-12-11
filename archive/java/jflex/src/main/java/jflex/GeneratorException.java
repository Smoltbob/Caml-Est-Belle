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
 * Thrown when code generation has to be aborted.
 *
 * @author Gerwin Klein
 * @version JFlex 1.6, $Revision: 815 $, $Date: 2014-05-19 03:07:52 +1000 (Mon, 19 May 2014) $
 */
public class GeneratorException extends RuntimeException {

  /**
	 * 
	 */
	private static final long serialVersionUID = -9128247888544263982L;

public GeneratorException() {
    super("Generation aborted");
  }

}
