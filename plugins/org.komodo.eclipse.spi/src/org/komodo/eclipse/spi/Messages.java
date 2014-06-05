/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.eclipse.spi;

import org.eclipse.osgi.util.NLS;
import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class Messages extends NLS implements StringConstants {
    private static final String BUNDLE_NAME = Messages.class.getPackage().getName()
																						+ DOT
																						+ Messages.class.getSimpleName().toLowerCase();

    public static String NoExecutionAdminFactory;

    public static String NoRegisteredExtension;
    
    static {
        // initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, Messages.class);
    }

    private Messages() {
    }
}
