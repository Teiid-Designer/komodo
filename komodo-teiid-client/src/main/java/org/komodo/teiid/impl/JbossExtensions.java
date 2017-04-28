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
package org.komodo.teiid.impl;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.jboss.as.cli.Util;
import org.jboss.as.cli.operation.impl.DefaultOperationRequestBuilder;
import org.jboss.as.controller.client.ModelControllerClient;
import org.jboss.dmr.ModelNode;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.teiid.ExtensionConstants;
import org.komodo.teiid.Messages;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.jboss.MetadataMapper;

public class JbossExtensions implements ExtensionConstants {

    private Object getField(Admin admin, String fieldName) throws Exception {
        if (admin == null)
            return null;

        try {
            Field field = admin.getClass().getDeclaredField(fieldName);
            field.setAccessible(true);
            return field.get(admin);
        } catch (NoSuchFieldException ex) {
            throw new Exception(Messages.getString(Messages.TeiidInstance.noSuchField, fieldName));
        }
    }

    /**
     * @param admin
     * @return server is in domain mode
     * @throws Exception
     */
    private boolean isDomainMode(Admin admin) throws Exception {
        if (admin == null)
            return false;

        Object domainMode = getField(admin, DOMAIN_MODE_FIELD);
        if (domainMode instanceof Boolean)
            return (Boolean) domainMode;
        else
            return false;
    }

    /**
     * @param admin
     * @return the underlying connection
     * @throws Exception
     */
    private ModelControllerClient getConnection(Admin admin) throws Exception {
        if (admin == null)
            throw new Exception("Client is not yet connected");

        Object connection = getField(admin, CONNECTION_FIELD);
        if (connection instanceof ModelControllerClient)
            return (ModelControllerClient) connection;

        throw new Exception(Messages.getString(Messages.TeiidInstance.noSuchField, CONNECTION_FIELD));
    }

    /**
     * @param admin
     * @param subsystem
     * @param childNode
     * @return child node names
     * @throws Exception
     */
    private List<String> getChildNodeNames(Admin admin, String subsystem, String childNode) throws Exception {
        final ModelNode request = buildRequest(admin, subsystem, READ_CHILDREN_NAMES, CHILD_TYPE, childNode);//$NON-NLS-1$
        ModelNode outcome = getConnection(admin).execute(request);
        if (Util.isSuccess(outcome)) {
            return Util.getList(outcome);
        }

        return Collections.emptyList();
    }

    /**
     * @param admin
     * @return profile name
     * @throws Exception
     */
    private String getProfileName(Admin admin) throws Exception {
        if (!isDomainMode(admin))
            return null;

        return getChildNodeNames(admin, null, PROFILE).get(0);
    }

    private void addProfileNode(Admin admin, DefaultOperationRequestBuilder builder) throws Exception {
        if (! isDomainMode(admin))
            return;

        String profile = getProfileName(admin);
        if (profile != null)
            builder.addNode(PROFILE, profile);
    }

    /**
     * @param admin
     * @param subsystem
     * @param operationName
     * @param params
     * @return a new model node request
     * @throws Exception
     */
    private ModelNode buildRequest(Admin admin, String subsystem, String operationName, String... params) throws Exception {
        DefaultOperationRequestBuilder builder = new DefaultOperationRequestBuilder();
        final ModelNode request;
        try {
            if (subsystem != null) {
                addProfileNode(admin, builder);
                builder.addNode(SUBSYSTEM, subsystem); //$NON-NLS-1$
            }
            builder.setOperationName(operationName);
            request = builder.buildRequest();
            if (params != null && params.length % 2 == 0) {
                for (int i = 0; i < params.length; i+=2) {
                    builder.addProperty(params[i], params[i+1]);
                }
            }
        } catch (Exception e) {
            throw new Exception(Messages.getString(Messages.TeiidInstance.buildOperationFailure, operationName), e); //$NON-NLS-1$
        }

        return request;
    }

    /**
     * @param operationResult
     * @param mapper
     * @return list of results from operation
     */
    private <T> List<T> getList(ModelNode operationResult,  MetadataMapper<T> mapper) {
        if(!operationResult.hasDefined(RESULT)) {
            return Collections.emptyList();
        }

        List<ModelNode> nodeList = operationResult.get(RESULT).asList(); //$NON-NLS-1$
        if(nodeList.isEmpty()) {
            return Collections.emptyList();
        }

        List<T> list = new ArrayList<T>(nodeList.size());
        for(ModelNode node : nodeList) {
            list.add(mapper.unwrap(node));
        }
        return list;
    }

    /**
     * @param admin
     * @return the drivers available on the underlying jboss server
     * @throws Exception
     */
    public Collection<ConnectionDriver> getDataSourceDrivers(Admin admin) throws Exception {
        if (admin == null)
            return Collections.emptyList();

        Set<ConnectionDriver> dataSourceDrivers = new HashSet<ConnectionDriver>();

        if (! isDomainMode(admin)) {
            //'installed-driver-list' not available in the domain mode.
            final ModelNode request = buildRequest(admin, DATA_SOURCES, INSTALLED_DRIVERS_LIST);
            ModelNode outcome = getConnection(admin).execute(request);
            if (Util.isSuccess(outcome)) {
                List<ConnectionDriver> drivers = getList(outcome, new MetadataMapper<ConnectionDriver>() {
                    @Override
                    public ConnectionDriver unwrap(ModelNode node) {
                        String driverName = null;
                        String driverClassName = null;
                        if (node.hasDefined(DRIVER_NAME))
                            driverName = node.get(DRIVER_NAME).asString();

                        if (node.hasDefined(DRIVER_CLASS_NAME))
                            driverClassName = node.get(DRIVER_CLASS_NAME).asString();

                        if (driverName == null || driverClassName == null)
                            return null;

                        return new ConnectionDriver(driverName, driverClassName);
                    }

                    @Override
                    public ModelNode describe(ModelNode node) {
                        throw new UnsupportedOperationException();
                    }

                    @Override
                    public ModelNode wrap(ConnectionDriver obj, ModelNode node) {
                        throw new UnsupportedOperationException();
                    }
                });

                dataSourceDrivers.addAll(drivers);
            } else {
                String msg = Messages.getString(Messages.TeiidInstance.requestDriverFailure, Util.getFailureDescription(outcome));
                throw new Exception(msg);
            }
        }
        else {
            // TODO: AS7 needs to provide better way to query the deployed JDBC drivers
            List<String> deployments = getChildNodeNames(admin, null, DEPLOYMENT);
            for (String deployment:deployments) {
                if (!deployment.contains(TRANSLATOR) && deployment.endsWith(DOT + JAR)) {
                    dataSourceDrivers.add(new ConnectionDriver(deployment, deployment));
                }
            }
        }
        return dataSourceDrivers;
    }

    public TeiidVersion getTeiidRuntimeVersion(Admin admin) throws Exception {
        if (admin == null)
            throw new Exception(Messages.getString(Messages.TeiidInstance.requestTeiidVersionFailure));

        final ModelNode request = buildRequest(admin, TEIID, READ_ATTRIBUTE, NAME, TEIID_RUNTIME_VERSION);
        ModelNode outcome = getConnection(admin).execute(request);
        if (Util.isSuccess(outcome)) {
            ModelNode result = outcome.get(RESULT);
            String versionId = result.asString();
            return new DefaultTeiidVersion(versionId);
        }

        throw new Exception(Messages.getString(Messages.TeiidInstance.requestTeiidVersionFailure));
    }
}
