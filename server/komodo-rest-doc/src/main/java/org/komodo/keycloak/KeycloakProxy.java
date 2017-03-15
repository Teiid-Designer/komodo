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
package org.komodo.keycloak;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import com.google.gson.stream.JsonWriter;

public class KeycloakProxy extends HttpServlet {

    private static class Property {
        private String envName;

        private String keycloakName;

        private String defaultValue;

        public Property(String envName, String keycloakName, String defaultValue) {
            this.envName = envName;
            this.keycloakName = keycloakName;
            this.defaultValue = defaultValue;
        }

        public String envName() {
            return envName;
        }

        public String keycloakName() {
            return keycloakName;
        }

        public String defaultValue() {
            return defaultValue;
        }
    }

    private static Property URL = new Property("vdb-builder.auth.server.url", "auth-server-url", "https://localhost:8543/auth");

    private static Property REALM = new Property("vdb-builder.auth.realm", "realm", "vdb");

    private static Property RESOURCE = new Property("vdb-builder-doc.auth.resource", "resource", "vdb-builder-doc");

    private static final long serialVersionUID = 1L;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("application/json");

        PrintWriter writer = response.getWriter();

        StringWriter sw = null;
        JsonWriter configWriter = null;;

        try {
            sw = new StringWriter();
            configWriter = new JsonWriter(sw);

            //        {
            //            "realm": "vdb",
            //            "auth-server-url": "https://localhost:8543/auth",
            //            "resource": "vdb-builder-doc",
            //        }

            configWriter.setIndent("    ");
            configWriter.setHtmlSafe(true);

            configWriter.beginObject();

            configWriter.name(URL.keycloakName());
            configWriter.value(System.getProperty(URL.envName(), URL.defaultValue()));

            configWriter.name(REALM.keycloakName());
            configWriter.value(System.getProperty(REALM.envName(), REALM.defaultValue()));

            configWriter.name(RESOURCE.keycloakName());
            configWriter.value(System.getProperty(RESOURCE.envName(), RESOURCE.defaultValue()));

            configWriter.endObject();

            writer.write(sw.toString());
        } finally {
            if (configWriter != null)
                configWriter.close();

            if (sw != null)
                sw.close();
        }
    }
}
