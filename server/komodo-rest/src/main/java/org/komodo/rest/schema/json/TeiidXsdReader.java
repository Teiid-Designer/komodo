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
package org.komodo.rest.schema.json;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.SAXParserFactory;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.StringUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
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
@SuppressWarnings( "nls" )
public class TeiidXsdReader implements XSVisitor, JsonConstants {

    private static final String SCHEMA = "schema-"; //$NON-NLS-1$

    private static final String TEIID_VDB_XSD = "teiid-vdb.xsd"; //$NON-NLS-1$

    private static final String SUGGESTED = "suggested"; //$NON-NLS-1$

    private static final String XSD_PROPERTY = "property"; //$NON-NLS-1$

    private ObjectMapper mapper;

    private ObjectNode rootNode;

    private ObjectNode parentCtx;

    private ObjectNode schemaCtx;

    private static enum Namespaces {
        VDB,
        MMCORE;

        @Override
        public String toString() {
            return name().toLowerCase() + PREFIX_SEPARATOR;
        }
    }

    private static class AliasMapper {

        private String element;

        private Map<String, String> xsdKengMap = new HashMap<String, String>();

        public AliasMapper(String element) {
            this.element = element;
        }

        public String getElement() {
            return this.element;
        }

        public String getKName(String xsdName) {
            String kName = xsdKengMap.get(xsdName);
            return kName == null ? xsdName : kName;
        }

        public void add(String xsdName, Namespaces prefix) {
            xsdKengMap.put(xsdName, prefix.toString() + xsdName);
        }

        public void add(String xsdName, Namespaces prefix, String kName) {
            xsdKengMap.put(xsdName, prefix.toString() + kName);
        }
    }

    private static class AliasMapperAdapter implements KeyFromValueAdapter<String, AliasMapper> {
        @Override
        public String getKey(AliasMapper value) {
            return value.getElement();
        }
    }

    private static KeyInValueHashMap<String, AliasMapper> ALIAS_CACHE = new KeyInValueHashMap<>(new AliasMapperAdapter());

    static {
        AliasMapper vdbMapper = new AliasMapper("vdb");
        vdbMapper.add("name", Namespaces.VDB);
        vdbMapper.add("description", Namespaces.VDB);
        vdbMapper.add("version", Namespaces.VDB);
        vdbMapper.add("connectionType", Namespaces.VDB);
        ALIAS_CACHE.add(vdbMapper);

        AliasMapper modelMapper = new AliasMapper("model");
        modelMapper.add("description", Namespaces.VDB);
        modelMapper.add("visible", Namespaces.VDB);
        modelMapper.add("path", Namespaces.VDB, "pathInVdb");
        modelMapper.add("type", Namespaces.MMCORE, "modelType");
        ALIAS_CACHE.add(modelMapper);

        AliasMapper sourceMapper = new AliasMapper("source");
        sourceMapper.add("translatorName", Namespaces.VDB, "sourceTranslator");
        sourceMapper.add("connectionJndiName", Namespaces.VDB, "sourceJndiName");
        ALIAS_CACHE.add(sourceMapper);

        AliasMapper translatorMapper = new AliasMapper("translator");
        translatorMapper.add("description", Namespaces.VDB);
        translatorMapper.add("type", Namespaces.VDB);
        ALIAS_CACHE.add(translatorMapper);

        AliasMapper dataRoleMapper = new AliasMapper("dataRole");
        dataRoleMapper.add("description", Namespaces.VDB);
        dataRoleMapper.add("anyAuthenticated", Namespaces.VDB);
        dataRoleMapper.add("allowCreateTemporaryTables", Namespaces.VDB);
        dataRoleMapper.add("grantAll", Namespaces.VDB);
        dataRoleMapper.add("mappedRoleName", Namespaces.VDB, "mappedRolesNames");
        ALIAS_CACHE.add(dataRoleMapper);

        AliasMapper permissionMapper = new AliasMapper("permission");
        permissionMapper.add("resourceName", Namespaces.VDB);
        permissionMapper.add("allowCreate", Namespaces.VDB);
        permissionMapper.add("allowRead", Namespaces.VDB);
        permissionMapper.add("allowUpdate", Namespaces.VDB);
        permissionMapper.add("allowDelete", Namespaces.VDB);
        permissionMapper.add("allowExecute", Namespaces.VDB);
        permissionMapper.add("allowAlter", Namespaces.VDB);
        permissionMapper.add("allowLanguage", Namespaces.VDB);
        permissionMapper.add("allowExecute", Namespaces.VDB);
        ALIAS_CACHE.add(permissionMapper);

        AliasMapper conditionMapper = new AliasMapper("condition");
        conditionMapper.add("constraint", Namespaces.VDB);
        ALIAS_CACHE.add(conditionMapper);

        AliasMapper maskMapper = new AliasMapper("mask");
        maskMapper.add("order", Namespaces.VDB);
        ALIAS_CACHE.add(maskMapper);

        AliasMapper entryMapper = new AliasMapper("entry");
        entryMapper.add("description", Namespaces.VDB);
        entryMapper.add("path", Namespaces.VDB);
        ALIAS_CACHE.add(entryMapper);

        AliasMapper importVdbMapper = new AliasMapper("importVdb");
        importVdbMapper.add("version", Namespaces.VDB);
        importVdbMapper.add("importDataPolicies", Namespaces.VDB);
        ALIAS_CACHE.add(importVdbMapper);
    }

    private String toLowerCamelCase(final String value) {
        StringBuffer sb = new StringBuffer();
        String[] values = value.split(HYPHEN);
        for (int i = 0; i < values.length; ++i) {
            String s = values[i];
            if (i == 0) {
                sb.append(s.toLowerCase());
                continue;
            }

            sb.append(Character.toUpperCase(s.charAt(0)));
            if (s.length() > 1) {
                sb.append(s.substring(1, s.length()).toLowerCase());
            }
        }

        return sb.toString();
     }

    private String convertPropertyName(String xsdName) {
        if (xsdName == null)
            return xsdName;

        xsdName = toLowerCamelCase(xsdName);
        JsonNode parentIdNode = parentCtx.get(ID);
        if (parentIdNode == null)
            return xsdName;

        String id = parentIdNode.asText();

        AliasMapper aliasMapper = ALIAS_CACHE.get(id);
        if (aliasMapper == null)
            return xsdName;

        return aliasMapper.getKName(xsdName);
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

        String description = annoObj.toString();
        description = description.replaceAll("[\\s]+", SPACE); //$NON-NLS-1$
        parentCtx.put(DESCRIPTION, description);
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

        String attributeName = convertPropertyName(decl.getName());
        attributeName = checkSuggestedPrefix(attributeName);
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
        String name = convertPropertyName(attributeDecl.getName());
        name = checkSuggestedPrefix(name);
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
            typeName = decl.getName();

        typeName = toLowerCamelCase(typeName);

        ObjectNode elementNode = null;
        if (type.getBaseType().getName().equals(XSD_PROPERTY)) {
            ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);
            elementNode = propertiesCtx.with(XSD_PROPERTY);
            parentCtx = elementNode;

        } else if (type.isComplexType()) {
            elementNode = parentCtx.objectNode();
            schemaCtx.set(typeName, elementNode);
            parentCtx = elementNode;

            elementNode.put(ID, toLowerCamelCase(decl.getName()));

            KomodoType kType = KomodoType.getKomodoType(typeName);
            elementNode.put(KTYPE, kType.getType());

            if (parent != schemaCtx) {
                ArrayNode children = parent.withArray(CHILDREN);
                children.add(typeName);
            }

        } else if (type.isSimpleType()) {
            //
            // Simple types get treated the same as attributes and added
            // to the properties array
            //
            ObjectNode propertiesCtx = parentCtx.with(PROPERTIES);
            String name = convertPropertyName(decl.getName());
            elementNode = propertiesCtx.with(name);
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

    private String writeNode(JsonNode node) throws JsonProcessingException {
        return getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(node);
    }

    private ObjectMapper getMapper() {
        if (mapper == null) {
            mapper = new ObjectMapper();
            mapper.enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
            mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
        }

        return mapper;
    }

    /**
     * @return String representation of teiid xsd
     * @throws Exception if error occurs
     */
    public String read() throws Exception {
        XSOMParser parser = new XSOMParser(SAXParserFactory.newInstance());
        parser.setAnnotationParser(new XsdAnnotationFactory());
        InputStream stream = this.getClass().getResourceAsStream(TEIID_VDB_XSD);
        if (stream == null)
            throw new FileNotFoundException();

        try {
            parser.parse(stream);

            XSSchemaSet schemaSet = parser.getResult();
            Collection<XSSchema> schemas = schemaSet.getSchemas();
            rootNode = getMapper().createObjectNode();

            Iterator<XSSchema> iterator = schemas.iterator();
            for (int i = 0; iterator.hasNext(); ++i) {
                XSSchema schema = iterator.next();

                parentCtx = rootNode;
                schema.visit(this);

                if (parentCtx != rootNode)
                    rootNode.set(SCHEMA + i, parentCtx);
            }

            return writeNode(rootNode);
        } finally {
            stream.close();
        }
    }

    private String schemaByKType(JsonNode node, KomodoType kType) throws Exception {
        if (node == null)
            throw new IllegalStateException("Programming error. read() should be called prior to this method");

        JsonNode jsonNode = node.get(KTYPE);
        if (jsonNode != null) {
            String kValue = jsonNode.asText();
            KomodoType nodeKType = KomodoType.getKomodoType(kValue);
            if (kType.equals(nodeKType))
                return writeNode(node);
        }

        Iterator<JsonNode> elements = node.elements();
        while (elements.hasNext()) {
            JsonNode element = elements.next();
            String schema = schemaByKType(element, kType);
            if (! EMPTY_STRING.equals(schema))
                return schema;
        }

        return EMPTY_STRING;
    }

    /**
     * @param kType the {@link KomodoType}
     * @return the schema representation for the given {@link KomodoType}
     * @throws Exception if error occurs
     */
    public String schemaByKType(KomodoType kType) throws Exception {
        if (rootNode == null)
            read();

        return schemaByKType(rootNode, kType);
    }

    /**
     * @param args arguments
     * @throws Exception if error occurs
     */
    public static void main(String[] args) throws Exception {
        TeiidXsdReader reader = new TeiidXsdReader();
        System.out.println(reader.read());
        System.out.println();
        System.out.println();
        System.out.println(reader.schemaByKType(KomodoType.MODEL));
        System.out.println();
        System.out.println();
        System.out.println(reader.schemaByKType(KomodoType.VDB_DATA_ROLE));

        String[] values = {
           KomodoType.VDB.getType(),
           KomodoType.VDB_IMPORT.getType(),
           KomodoType.VDB_TRANSLATOR.getType(),
           KomodoType.MODEL.getType(),
           KomodoType.VDB_MODEL_SOURCE.getType(),
           KomodoType.VDB_DATA_ROLE.getType(),
           KomodoType.VDB_PERMISSION.getType(),
           KomodoType.VDB_CONDITION.getType(),
           KomodoType.VDB_MASK.getType(),
        };
        System.out.println(StringUtils.toCommaSeparatedList(values));
    }
}
