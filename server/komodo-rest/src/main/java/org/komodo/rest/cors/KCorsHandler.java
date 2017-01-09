/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.cors;

import java.util.Set;
import org.komodo.spi.constants.StringConstants;

/**
 * Handler which intercepts / filters to resolve CORS-related issues
 * with client-server communication
 */
public interface KCorsHandler extends StringConstants {
    
    String ALLOW_HEADERS = "Content-Type, X-Requested-With, accept, Origin," + //$NON-NLS-1$
                                                     "Access-Control-Request-Method," + //$NON-NLS-1$
                                                     "Access-Control-Request-Headers, Authorization"; //$NON-NLS-1$

    String ALLOW_METHODS = "GET, POST, PUT, DELETE, OPTIONS, HEAD"; //$NON-NLS-1$

    /**
     * @return approved origins
     */
    Set<String> getAllowedOrigins();

    /**
     * @return true if credentials are allowed, false otherwise
     */
    boolean isAllowCredentials();

    /**
     * @param allowCredentials
     */
    void setAllowCredentials(boolean allowCredentials);

    /**
     * @return the Access-Control-Allow-Methods
     */
    String getAllowedMethods();

    /**
     * Comma delimited string for Access-Control-Allow-Methods
     *
     * @param allowedMethods
     */
    void setAllowedMethods(String allowedMethods);

    /**
     * @return the Access-Control-Allow-Headers
     */
    String getAllowedHeaders();

    /**
     * Comma delimited string for Access-Control-Allow-Headers
     *
     * @param allowedHeaders
     */
    void setAllowedHeaders(String allowedHeaders);

    /**
     * @return max age
     */
    int getCorsMaxAge();

    /**
     * @param corsMaxAge
     */
    void setCorsMaxAge(int corsMaxAge);

    /**
     * @return the exposed headers
     */
    String getExposedHeaders();

    /**
     * @param exposedHeaders
     */
    void setExposedHeaders(String exposedHeaders);
}
