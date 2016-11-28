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

import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import org.jboss.resteasy.spi.DefaultOptionsMethodException;
import org.komodo.spi.constants.StringConstants;

@Provider
public class OptionsExceptionMapper implements ExceptionMapper<DefaultOptionsMethodException>, StringConstants {

    @Override
    public Response toResponse(DefaultOptionsMethodException exception) {
        //
        // Exception provides its own OK response to avoid this exception causing failures
        //
        Response response = exception.getResponse();

        MultivaluedMap<String, Object> headers = response.getMetadata();
        headers.add(CorsHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, STAR);
        headers.add(CorsHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
        headers.add(CorsHeaders.ACCESS_CONTROL_ALLOW_METHODS, KCorsHandler.ALLOW_METHODS);
        headers.add(CorsHeaders.ACCESS_CONTROL_ALLOW_HEADERS, KCorsHandler.ALLOW_HEADERS);
        headers.add(CorsHeaders.ACCESS_CONTROL_MAX_AGE, 1209600);

        return response;
    }

}
