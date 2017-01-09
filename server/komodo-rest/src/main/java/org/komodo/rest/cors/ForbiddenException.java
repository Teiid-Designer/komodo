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

import javax.ws.rs.core.Response;
import org.jboss.resteasy.spi.LoggableFailure;

public class ForbiddenException extends LoggableFailure {

    private static final long serialVersionUID = -7370671758722865878L;

    public ForbiddenException() {
        super(403);
    }

    public ForbiddenException(String s) {
        super(s, 403);
    }

    public ForbiddenException(String s, Response response) {
        super(s, response);
    }

    public ForbiddenException(String s, Throwable throwable, Response response) {
        super(s, throwable, response);
    }

    public ForbiddenException(String s, Throwable throwable) {
        super(s, throwable, 403);
    }

    public ForbiddenException(Throwable throwable) {
        super(throwable, 403);
    }

    public ForbiddenException(Throwable throwable, Response response) {
        super(throwable, response);
    }
}
