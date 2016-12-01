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
package org.komodo.rest.cors.rest3;

import org.komodo.rest.cors.KCorsHandler;

/**
 * Class to exploit the rest 3 CorsFilter and implement
 * the {@link KCorsHandler} to allow the factory to instantiate it
 *
 * Only compiled against resteasy 3 libraries. Excluded for resteasy 2
 */
public class KCorsFilter extends org.jboss.resteasy.plugins.interceptors.CorsFilter implements KCorsHandler {

    // Nothing else required

}
