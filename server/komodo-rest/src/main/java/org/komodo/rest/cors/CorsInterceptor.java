package org.komodo.rest.cors;
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

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;
import org.jboss.resteasy.annotations.interception.ServerInterceptor;
import org.jboss.resteasy.core.ResourceMethod;
import org.jboss.resteasy.core.ServerResponse;
import org.jboss.resteasy.spi.Failure;
import org.jboss.resteasy.spi.HttpRequest;
import org.jboss.resteasy.spi.interception.PostProcessInterceptor;
import org.jboss.resteasy.spi.interception.PreProcessInterceptor;
import org.komodo.spi.constants.StringConstants;

@Provider
@ServerInterceptor
public class CorsInterceptor implements PreProcessInterceptor, PostProcessInterceptor, StringConstants {

    public static final String ALLOW_HEADERS = "Content-Type, X-Requested-With, accept, Origin," + //$NON-NLS-1$
                                                                                        "Access-Control-Request-Method," + //$NON-NLS-1$
                                                                                        "Access-Control-Request-Headers, Authorization"; //$NON-NLS-1$

    public static final String ALLOW_METHODS = "GET, POST, PUT, DELETE, OPTIONS, HEAD"; //$NON-NLS-1$

    private boolean allowCredentials = true;

    private String allowedMethods;

    private String allowedHeaders;

    private String exposedHeaders;

    private int corsMaxAge = -1;

    private Set<String> allowedOrigins = new HashSet<String>();

    private static final ThreadLocal<String> REQUEST_ORIGIN = new ThreadLocal<String>();

    /**
     * Put "*" if you want to accept all origins
     *
     * @return
     */
    public Set<String> getAllowedOrigins() {
        return allowedOrigins;
    }

    /**
     * Defaults to true
     *
     * @return
     */
    public boolean isAllowCredentials() {
        return allowCredentials;
    }

    public void setAllowCredentials(boolean allowCredentials) {
        this.allowCredentials = allowCredentials;
    }

    /**
     * Will allow all by default
     *
     * @return
     */
    public String getAllowedMethods() {
        return allowedMethods;
    }

    /**
     * Will allow all by default
     * comma delimited string for Access-Control-Allow-Methods
     *
     * @param allowedMethods
     */
    public void setAllowedMethods(String allowedMethods) {
        this.allowedMethods = allowedMethods;
    }

    public String getAllowedHeaders() {
        return allowedHeaders;
    }

    /**
     * Will allow all by default
     * comma delimited string for Access-Control-Allow-Headers
     *
     * @param allowedHeaders
     */
    public void setAllowedHeaders(String allowedHeaders) {
        this.allowedHeaders = allowedHeaders;
    }

    public int getCorsMaxAge() {
        return corsMaxAge;
    }

    public void setCorsMaxAge(int corsMaxAge) {
        this.corsMaxAge = corsMaxAge;
    }

    public String getExposedHeaders() {
        return exposedHeaders;
    }

    /**
     * comma delimited list
     *
     * @param exposedHeaders
     */
    public void setExposedHeaders(String exposedHeaders) {
        this.exposedHeaders = exposedHeaders;
    }

    @Override
    public ServerResponse preProcess(HttpRequest request, ResourceMethod method) throws Failure, WebApplicationException {
        HttpHeaders httpHeaders = request.getHttpHeaders();
        MultivaluedMap<String, String> headers = httpHeaders.getRequestHeaders();
        String origin = headers.getFirst(CorsHeaders.ORIGIN);

        //
        // Need to stash this for the post process
        //
        REQUEST_ORIGIN.set(origin);

        if (origin == null)
            return null;

        if (isOption(method.getMethod())) {
            return preflight(origin, request);

        } else {
            checkOrigin(request, origin);
        }

        return null;
    }

    private boolean isOption(Method method) {
        if (method == null)
            return false;

        return method.getName().equalsIgnoreCase("OPTIONS");
    }

    @Override
    public void postProcess(ServerResponse response) {
        MultivaluedMap<String, Object> headers = response.getMetadata();
        String origin = REQUEST_ORIGIN.get();

        if (origin == null || isOption(response.getResourceMethod())) {
            // don't do anything if origin is null, its an OPTIONS request, or cors.failure is set
            return;
        }

        headers.putSingle(CorsHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, origin);

        if (allowCredentials)
            headers.putSingle(CorsHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");

        if (exposedHeaders != null)
            headers.putSingle(CorsHeaders.ACCESS_CONTROL_EXPOSE_HEADERS, exposedHeaders);
    }

    protected ServerResponse preflight(String origin, HttpRequest requestContext) {
        checkOrigin(requestContext, origin);

        Response.ResponseBuilder builder = Response.ok();
        builder.header(CorsHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, origin);
        if (allowCredentials)
            builder.header(CorsHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");

        HttpHeaders httpHeaders = requestContext.getHttpHeaders();
        MultivaluedMap<String, String> headers = httpHeaders.getRequestHeaders();

        String requestMethods = headers.getFirst(CorsHeaders.ACCESS_CONTROL_REQUEST_METHOD);
        if (requestMethods != null) {
            if (allowedMethods != null) {
                requestMethods = this.allowedMethods;
            }
            builder.header(CorsHeaders.ACCESS_CONTROL_ALLOW_METHODS, requestMethods);
        }

        String allowHeaders = headers.getFirst(CorsHeaders.ACCESS_CONTROL_REQUEST_HEADERS);
        if (allowHeaders != null) {
            if (allowedHeaders != null) {
                allowHeaders = this.allowedHeaders;
            }
            builder.header(CorsHeaders.ACCESS_CONTROL_ALLOW_HEADERS, allowHeaders);
        }

        if (corsMaxAge > -1) {
            builder.header(CorsHeaders.ACCESS_CONTROL_MAX_AGE, corsMaxAge);
        }

        Response response = builder.build();
        return ServerResponse.copyIfNotServerResponse(response);
    }

    protected void checkOrigin(HttpRequest requestContext, String origin) {
        if (allowedOrigins.contains(STAR))
            return; // Nothing to check all good

        if (origin == null)
            return;

        if (!allowedOrigins.contains(origin)) {
            requestContext.setAttribute("cors.failure", true);
            throw new ForbiddenException("Origin not allowed: " + origin);
        }
    }
}
