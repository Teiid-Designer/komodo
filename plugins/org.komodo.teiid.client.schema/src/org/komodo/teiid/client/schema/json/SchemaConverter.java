package org.komodo.teiid.client.schema.json;

import org.komodo.teiid.client.schema.Vdb;
import com.fasterxml.jackson.databind.AnnotationIntrospector;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.fasterxml.jackson.module.jsonSchema.JsonSchema;
import com.fasterxml.jackson.module.jsonSchema.factories.SchemaFactoryWrapper;

/**
 * Generates JavaScript Object Notation (JSON) from Java classes
 * with Java API for XML Binding (JAXB) annotations.
 */
public class SchemaConverter {

    /**
     * @return String representation of {@link Vdb} schema
     *
     * @throws Exception if error occurs
     */
    public String convert() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        mapper.enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY);
        mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);

        SchemaFactoryWrapper visitor = new SchemaFactoryWrapper();
        TypeFactory typeFactory = TypeFactory.defaultInstance();

        AnnotationIntrospector primary = new JaxbAnnotationIntrospector(typeFactory);
        AnnotationIntrospector secondary = new JacksonAnnotationIntrospector();
        AnnotationIntrospector pair = AnnotationIntrospector.pair(primary, secondary);
        mapper.setAnnotationIntrospector(pair);

        mapper.acceptJsonFormatVisitor(mapper.constructType(Vdb.class), visitor);
        JsonSchema jsonSchema = visitor.finalSchema();

        return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(jsonSchema);
    }

    /**
     * @param arguments (not required)
     * @throws Exception if error occurs
     */
    public static void main(final String[] arguments) throws Exception {
        final SchemaConverter instance = new SchemaConverter();
        String json = instance.convert();
        System.out.println(json);
    }
}
