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
package org.komodo.teiid.client.schema.json;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.xml.parsers.SAXParserFactory;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.sun.xml.xsom.XSAnnotation;
import com.sun.xml.xsom.XSAttGroupDecl;
import com.sun.xml.xsom.XSAttributeDecl;
import com.sun.xml.xsom.XSAttributeUse;
import com.sun.xml.xsom.XSComplexType;
import com.sun.xml.xsom.XSContentType;
import com.sun.xml.xsom.XSElementDecl;
import com.sun.xml.xsom.XSFacet;
import com.sun.xml.xsom.XSIdentityConstraint;
import com.sun.xml.xsom.XSModelGroup;
import com.sun.xml.xsom.XSModelGroupDecl;
import com.sun.xml.xsom.XSNotation;
import com.sun.xml.xsom.XSParticle;
import com.sun.xml.xsom.XSRestrictionSimpleType;
import com.sun.xml.xsom.XSSchema;
import com.sun.xml.xsom.XSSchemaSet;
import com.sun.xml.xsom.XSSimpleType;
import com.sun.xml.xsom.XSTerm;
import com.sun.xml.xsom.XSType;
import com.sun.xml.xsom.XSWildcard;
import com.sun.xml.xsom.XSXPath;
import com.sun.xml.xsom.parser.XSOMParser;
import com.sun.xml.xsom.visitor.XSVisitor;

/**
 *
 */
public class TeiidXsdReader implements XSVisitor, StringConstants {

    private static final String SCHEMA = "schema-"; //$NON-NLS-1$

    private static final String TEIID_VDB_XSD = "/org/komodo/teiid/client/schema/teiid-vdb.xsd"; //$NON-NLS-1$

    private static final String ID = "id"; //$NON-NLS-1$

    private static final String TYPE = "type"; //$NON-NLS-1$

    private static final String KTYPE = "k-type"; //$NON-NLS-1$

    private static final String DEFAULT_VALUE = "default-value"; //$NON-NLS-1$

    private static final String DESCRIPTION = "description"; //$NON-NLS-1$

    private static final String PROPERTIES = "properties"; //$NON-NLS-1$

    private static final String SUGGESTED = "suggested"; //$NON-NLS-1$

    private static final String PROPERTY = "property"; //$NON-NLS-1$

    private static final String REQUIRED = "required"; //$NON-NLS-1$

    private static final String REPEATABLE = "repeatable"; //$NON-NLS-1$

    private static final String LIMIT = "limit"; //$NON-NLS-1$

    private static final String VALUES = "values"; //$NON-NLS-1$

    private static final String CHILDREN = "children"; //$NON-NLS-1$

    private ObjectMapper mapper;

    private ObjectNode rootNode;

    private ObjectNode parentCtx;

    private ObjectNode schemaCtx;

    private String capitalize(final String line) {
        return Character.toUpperCase(line.charAt(0)) + line.substring(1);
     }

    @Override
    public void wildcard(XSWildcard wc) {
        // Not required
    }

    @Override
    public void modelGroupDecl(XSModelGroupDecl decl) {
        // Not required
    }

    @Override
    public void empty(XSContentType empty) {
        // Not required
    }

    @Override
    public void annotation(XSAnnotation ann) {
        if(ann == null)
            return;

        Object annoObj = ann.getAnnotation();
        if(annoObj == null)
            return;

        parentCtx.put(DESCRIPTION, annoObj.toString());
    }

    @Override
    public void attGroupDecl(XSAttGroupDecl decl) {
        // Not required
    }

    @Override
    public void facet(XSFacet facet) {
        // Not required
    }

    @Override
    public void notation(XSNotation notation) {
        // Not required
    }

    @Override
    public void identityConstraint(XSIdentityConstraint decl) {
        // Not required
    }

    @Override
    public void xpath(XSXPath xp) {
        // Not required
    }

    private String checkSuggestedPrefix(String name) {
        if (name.startsWith(SUGGESTED)) {
            name = name.substring(SUGGESTED.length()).toLowerCase();
        }

        return name;
    }

    @Override
    public void attributeDecl(XSAttributeDecl decl) {
        ObjectNode parent = parentCtx;

        ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);

        String attributeName = checkSuggestedPrefix(decl.getName());

        ObjectNode attrNode = propertiesCtx.with(attributeName);

        if (decl.getDefaultValue() != null)
            attrNode.put(DEFAULT_VALUE, decl.getDefaultValue().value);

        parentCtx = attrNode;
        decl.getType().visit(this);

        parentCtx = attrNode;
        annotation(decl.getAnnotation());

        parentCtx = parent;
    }

    @Override
    public void attributeUse(XSAttributeUse use) {
        XSAttributeDecl attributeDecl = use.getDecl();
        attributeDecl.visit(this);

        ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);
        String name = checkSuggestedPrefix(use.getDecl().getName());
        ObjectNode attrNode = propertiesCtx.with(name);

        attrNode.put(REQUIRED, Boolean.toString(use.isRequired()));
    }

    @Override
    public void simpleType(XSSimpleType simpleType) {
        if (simpleType.isRestriction()) {
            XSRestrictionSimpleType restriction = simpleType.asRestriction();
            parentCtx.put(TYPE, restriction.getPrimitiveType().getName());

            Iterator<? extends XSFacet> i = restriction.getDeclaredFacets().iterator();
            while(i.hasNext()){
                XSFacet facet = i.next();
                if(facet.getName().equals(XSFacet.FACET_ENUMERATION)) {
                    ArrayNode values = parentCtx.withArray(VALUES);
                    if (values.size() == 0)
                        values.add(facet.getValue().toString());
                    else {
                        List<String> valList = new ArrayList<String>();
                        valList.add(facet.getValue().toString());

                        Iterator<JsonNode> iterator = values.elements();
                        while(iterator.hasNext()) {
                            String value = iterator.next().asText();
                            valList.add(value);
                        }

                        values.removeAll();

                        Collections.sort(valList);
                        for (String value : valList)
                            values.add(value);
                    }
                }
            }
        } else {
            parentCtx.put(TYPE, simpleType.getName());
        }
    }

    @Override
    public void modelGroup(XSModelGroup group) {
        XSParticle[] particles = group.getChildren();

        if (particles.length == 0)
            return;

        ObjectNode parent = parentCtx;

        for (XSParticle p : particles) {
            parentCtx = parent;
            p.visit(this);
        }

        parentCtx = parent;
    }

    @Override
    public void particle(XSParticle particle) {
        XSTerm term = particle.getTerm();
        term.visit(this);

        parentCtx.put(REQUIRED, Boolean.toString(particle.getMinOccurs().compareTo( BigInteger.ZERO) > 0));
        parentCtx.put(REPEATABLE, Boolean.toString(particle.isRepeated()));

        if (particle.isRepeated()) {
            parentCtx.put(LIMIT, particle.getMaxOccurs().toString());
        }
    }

    @Override
    public void complexType(XSComplexType complexType) {
        Iterator<? extends XSAttributeUse> attrUses = complexType.getAttributeUses().iterator();
        while(attrUses.hasNext()) {
            attrUses.next().visit(this);
        }

        complexType.getContentType().visit(this);
    }

    @Override
    public void elementDecl(XSElementDecl decl) {
        ObjectNode parent = parentCtx;

        XSType type = decl.getType();
        String typeName = type.getName();
        if (type.isLocal() || typeName == null)
            typeName = capitalize(decl.getName());

        ObjectNode elementNode = null;
        if (type.getBaseType().getName().equals(PROPERTY)) {
            ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);
            elementNode = propertiesCtx.with(PROPERTY);
            parentCtx = elementNode;

        } else if (type.isComplexType()) {
            elementNode = parentCtx.objectNode();
            schemaCtx.set(typeName, elementNode);
            parentCtx = elementNode;

            elementNode.put(ID, decl.getName());

            KomodoType kType = KomodoType.getKomodoType(typeName.toLowerCase());
            elementNode.put(KTYPE, kType.name());

            ArrayNode children = parent.withArray(CHILDREN);
            children.add(typeName);

        } else if (type.isSimpleType()) {
            //
            // Simple types get treated the same as attributes and added
            // to the properties array
            //
            ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);
            elementNode = propertiesCtx.with(decl.getName());
            parentCtx = elementNode;
        }

        annotation(decl.getAnnotation());
        decl.getType().visit(this);

        if (decl.getDefaultValue() != null)
            elementNode.put(DEFAULT_VALUE, decl.getDefaultValue().value);
    }

    @Override
    public void schema(XSSchema schema) {
        if (schema.getElementDecls().size() == 0)
            return;

        schemaCtx = parentCtx.objectNode();

        Iterator<XSElementDecl> iterator = schema.iterateElementDecls();
        while (iterator.hasNext()) {
            XSElementDecl element = iterator.next();
            parentCtx = schemaCtx;
            element.visit(this);
        }

        parentCtx = schemaCtx;
    }

    /**
     * @return String representation of teiid xsd
     * @throws Exception if error occurs
     */
    public String read() throws Exception {
        mapper = new ObjectMapper();
        mapper.enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
        mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);

        XSOMParser parser = new XSOMParser(SAXParserFactory.newInstance());
        parser.setAnnotationParser(new XsdAnnotationFactory());
        InputStream stream = this.getClass().getResourceAsStream(TEIID_VDB_XSD);
        if (stream == null)
            throw new FileNotFoundException();

        try {
            parser.parse(stream);

            XSSchemaSet schemaSet = parser.getResult();
            Collection<XSSchema> schemas = schemaSet.getSchemas();
            rootNode = mapper.createObjectNode();

            Iterator<XSSchema> iterator = schemas.iterator();
            for (int i = 0; iterator.hasNext(); ++i) {
                XSSchema schema = iterator.next();

                parentCtx = rootNode;
                schema.visit(this);

                if (parentCtx != rootNode)
                    rootNode.set(SCHEMA + i, parentCtx);
            }

            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(rootNode);
        } finally {
            stream.close();
        }
    }

    /**
     * @param args arguments
     * @throws Exception if error occurs
     */
    public static void main(String[] args) throws Exception {
        TeiidXsdReader reader = new TeiidXsdReader();
        System.out.println(reader.read());
    }
}
